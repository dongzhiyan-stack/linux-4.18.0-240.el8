/* SPDX-License-Identifier: GPL-2.0 */
#ifndef _IPV6_STUBS_H
#define _IPV6_STUBS_H

#include <linux/in6.h>
#include <linux/netdevice.h>
#include <linux/skbuff.h>
#include <net/dst.h>
#include <net/flow.h>
#include <net/neighbour.h>
#include <net/sock.h>

/* structs from net/ip6_fib.h */
struct fib6_info;

/* This is ugly, ideally these symbols should be built
 * into the core kernel.
 */
struct ipv6_stub {
	int (*ipv6_sock_mc_join)(struct sock *sk, int ifindex,
				 const struct in6_addr *addr);
	int (*ipv6_sock_mc_drop)(struct sock *sk, int ifindex,
				 const struct in6_addr *addr);
	struct dst_entry *(*ipv6_dst_lookup_flow)(struct net *net,
						  const struct sock *sk,
						  struct flowi6 *fl6,
						  const struct in6_addr *final_dst);
	int (*ipv6_route_input)(struct sk_buff *skb);

	struct fib6_table *(*fib6_get_table)(struct net *net, u32 id);
	struct fib6_info *(*fib6_lookup)(struct net *net, int oif,
					 struct flowi6 *fl6, int flags);
	struct fib6_info *(*fib6_table_lookup)(struct net *net,
					      struct fib6_table *table,
					      int oif, struct flowi6 *fl6,
					      int flags);
	struct fib6_info *(*fib6_multipath_select)(const struct net *net,
						   struct fib6_info *f6i,
						   struct flowi6 *fl6, int oif,
						   const struct sk_buff *skb,
						   int strict);
	u32 (*ip6_mtu_from_fib6)(struct fib6_info *f6i, struct in6_addr *daddr,
				 struct in6_addr *saddr);

	void (*fib6_rt_update)(struct net *net, struct fib6_info *rt,
			       struct nl_info *info);

	void (*udpv6_encap_enable)(void);
	void (*ndisc_send_na)(struct net_device *dev, const struct in6_addr *daddr,
			      const struct in6_addr *solicited_addr,
			      bool router, bool solicited, bool override, bool inc_opt);
#if IS_ENABLED(CONFIG_XFRM)
	int (*xfrm6_udp_encap_rcv)(struct sock *sk, struct sk_buff *skb);
	int (*xfrm6_rcv_encap)(struct sk_buff *skb, int nexthdr, __be32 spi,
			       int encap_type);
#endif
	struct neigh_table *nd_tbl;
};
extern const struct ipv6_stub *ipv6_stub __read_mostly;

/* A stub used by bpf helpers. Similarly ugly as ipv6_stub */
struct ipv6_bpf_stub {
	int (*inet6_bind)(struct sock *sk, struct sockaddr *uaddr, int addr_len,
			  bool force_bind_address_no_port, bool with_lock);
	struct sock *(*udp6_lib_lookup)(struct net *net,
				     const struct in6_addr *saddr, __be16 sport,
				     const struct in6_addr *daddr, __be16 dport,
				     int dif, int sdif, struct udp_table *tbl,
				     struct sk_buff *skb);
};
extern const struct ipv6_bpf_stub *ipv6_bpf_stub __read_mostly;

#endif
