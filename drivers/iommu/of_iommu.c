/*
 * OF helpers for IOMMU
 *
 * Copyright (c) 2012, NVIDIA CORPORATION.  All rights reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms and conditions of the GNU General Public License,
 * version 2, as published by the Free Software Foundation.
 *
 * This program is distributed in the hope it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <linux/export.h>
#include <linux/iommu.h>
#include <linux/limits.h>
#include <linux/module.h>
#include <linux/msi.h>
#include <linux/of.h>
#include <linux/of_iommu.h>
#include <linux/of_pci.h>
#include <linux/pci.h>
#include <linux/slab.h>

#define NO_IOMMU	1

static const struct of_device_id __iommu_of_table_sentinel
	__used __section(__iommu_of_table_end);

/**
 * of_get_dma_window - Parse *dma-window property and returns 0 if found.
 *
 * @dn: device node
 * @prefix: prefix for property name if any
 * @index: index to start to parse
 * @busno: Returns busno if supported. Otherwise pass NULL
 * @addr: Returns address that DMA starts
 * @size: Returns the range that DMA can handle
 *
 * This supports different formats flexibly. "prefix" can be
 * configured if any. "busno" and "index" are optionally
 * specified. Set 0(or NULL) if not used.
 */
int of_get_dma_window(struct device_node *dn, const char *prefix, int index,
		      unsigned long *busno, dma_addr_t *addr, size_t *size)
{
	const __be32 *dma_window, *end;
	int bytes, cur_index = 0;
	char propname[NAME_MAX], addrname[NAME_MAX], sizename[NAME_MAX];

	if (!dn || !addr || !size)
		return -EINVAL;

	if (!prefix)
		prefix = "";

	snprintf(propname, sizeof(propname), "%sdma-window", prefix);
	snprintf(addrname, sizeof(addrname), "%s#dma-address-cells", prefix);
	snprintf(sizename, sizeof(sizename), "%s#dma-size-cells", prefix);

	dma_window = of_get_property(dn, propname, &bytes);
	if (!dma_window)
		return -ENODEV;
	end = dma_window + bytes / sizeof(*dma_window);

	while (dma_window < end) {
		u32 cells;
		const void *prop;

		/* busno is one cell if supported */
		if (busno)
			*busno = be32_to_cpup(dma_window++);

		prop = of_get_property(dn, addrname, NULL);
		if (!prop)
			prop = of_get_property(dn, "#address-cells", NULL);

		cells = prop ? be32_to_cpup(prop) : of_n_addr_cells(dn);
		if (!cells)
			return -EINVAL;
		*addr = of_read_number(dma_window, cells);
		dma_window += cells;

		prop = of_get_property(dn, sizename, NULL);
		cells = prop ? be32_to_cpup(prop) : of_n_size_cells(dn);
		if (!cells)
			return -EINVAL;
		*size = of_read_number(dma_window, cells);
		dma_window += cells;

		if (cur_index++ == index)
			break;
	}
	return 0;
}
EXPORT_SYMBOL_GPL(of_get_dma_window);

static bool of_iommu_driver_present(struct device_node *np)
{
	/*
	 * If the IOMMU still isn't ready by the time we reach init, assume
	 * it never will be. We don't want to defer indefinitely, nor attempt
	 * to dereference __iommu_of_table after it's been freed.
	 */
	if (system_state >= SYSTEM_RUNNING)
		return false;

	return of_match_node(&__iommu_of_table, np);
}

static int of_iommu_xlate(struct device *dev,
			  struct of_phandle_args *iommu_spec)
{
	const struct iommu_ops *ops;
	struct fwnode_handle *fwnode = &iommu_spec->np->fwnode;
	int ret;

	ops = iommu_ops_from_fwnode(fwnode);
	if ((ops && !ops->of_xlate) ||
	    !of_device_is_available(iommu_spec->np) ||
	    (!ops && !of_iommu_driver_present(iommu_spec->np)))
		return NO_IOMMU;

	ret = iommu_fwspec_init(dev, &iommu_spec->np->fwnode, ops);
	if (ret)
		return ret;
	/*
	 * The otherwise-empty fwspec handily serves to indicate the specific
	 * IOMMU device we're waiting for, which will be useful if we ever get
	 * a proper probe-ordering dependency mechanism in future.
	 */
	if (!ops)
		return -EPROBE_DEFER;

	if (!try_module_get(ops->owner))
		return -ENODEV;

	ret = ops->of_xlate(dev, iommu_spec);
	module_put(ops->owner);
	return ret;
}

struct of_pci_iommu_alias_info {
	struct device *dev;
	struct device_node *np;
};

static int of_pci_iommu_init(struct pci_dev *pdev, u16 alias, void *data)
{
	struct of_pci_iommu_alias_info *info = data;
	struct of_phandle_args iommu_spec = { .args_count = 1 };
	int err;

	err = of_map_rid(info->np, alias, "iommu-map", "iommu-map-mask",
			 &iommu_spec.np, iommu_spec.args);
	if (err)
		return err == -ENODEV ? NO_IOMMU : err;

	err = of_iommu_xlate(info->dev, &iommu_spec);
	of_node_put(iommu_spec.np);
	return err;
}

const struct iommu_ops *of_iommu_configure(struct device *dev,
					   struct device_node *master_np)
{
	const struct iommu_ops *ops = NULL;
	struct iommu_fwspec *fwspec = dev_iommu_fwspec_get(dev);
	int err = NO_IOMMU;

	if (!master_np)
		return NULL;

	if (fwspec) {
		if (fwspec->ops)
			return fwspec->ops;

		/* In the deferred case, start again from scratch */
		iommu_fwspec_free(dev);
	}

	/*
	 * We don't currently walk up the tree looking for a parent IOMMU.
	 * See the `Notes:' section of
	 * Documentation/devicetree/bindings/iommu/iommu.txt
	 */
	if (dev_is_pci(dev)) {
		struct of_pci_iommu_alias_info info = {
			.dev = dev,
			.np = master_np,
		};

		pci_request_acs();
		err = pci_for_each_dma_alias(to_pci_dev(dev),
					     of_pci_iommu_init, &info);
	} else {
		struct of_phandle_args iommu_spec;
		int idx = 0;

		while (!of_parse_phandle_with_args(master_np, "iommus",
						   "#iommu-cells",
						   idx, &iommu_spec)) {
			err = of_iommu_xlate(dev, &iommu_spec);
			of_node_put(iommu_spec.np);
			idx++;
			if (err)
				break;
		}

		fwspec = dev_iommu_fwspec_get(dev);
		if (!err && fwspec)
			of_property_read_u32(master_np, "pasid-num-bits",
					     &fwspec->num_pasid_bits);
	}

	/*
	 * Two success conditions can be represented by non-negative err here:
	 * >0 : there is no IOMMU, or one was unavailable for non-fatal reasons
	 *  0 : we found an IOMMU, and dev->fwspec is initialised appropriately
	 * <0 : any actual error
	 */
	if (!err) {
		/* The fwspec pointer changed, read it again */
		fwspec = dev_iommu_fwspec_get(dev);
		ops    = fwspec->ops;
	}
	/*
	 * If we have reason to believe the IOMMU driver missed the initial
	 * probe for dev, replay it to get things in order.
	 */
	if (!err && dev->bus && !device_iommu_mapped(dev))
		err = iommu_probe_device(dev);

	/* Ignore all other errors apart from EPROBE_DEFER */
	if (err == -EPROBE_DEFER) {
		ops = ERR_PTR(err);
	} else if (err < 0) {
		dev_dbg(dev, "Adding to IOMMU failed: %d\n", err);
		ops = NULL;
	}

	return ops;
}
