/* SPDX-License-Identifier: GPL-2.0 */
#ifndef ASM_POWERPC_DMA_DIRECT_H
#define ASM_POWERPC_DMA_DIRECT_H 1

static inline dma_addr_t __phys_to_dma(struct device *dev, phys_addr_t paddr)
{
	if (!dev)
		return paddr + PCI_DRAM_OFFSET;
	return paddr + dev->archdata.dma_offset;
}

static inline phys_addr_t __dma_to_phys(struct device *dev, dma_addr_t daddr)
{
	if (!dev)
		return daddr - PCI_DRAM_OFFSET;
	return daddr - dev->archdata.dma_offset;
}
#endif /* ASM_POWERPC_DMA_DIRECT_H */
