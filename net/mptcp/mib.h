/* SPDX-License-Identifier: GPL-2.0-or-later */

enum linux_mptcp_mib_field {
	MPTCP_MIB_NUM = 0,
	MPTCP_MIB_MPCAPABLEPASSIVE,	/* Received SYN with MP_CAPABLE */
	MPTCP_MIB_MPCAPABLEPASSIVEACK,	/* Received third ACK with MP_CAPABLE */
	MPTCP_MIB_MPCAPABLEPASSIVEFALLBACK,/* Server-side fallback during 3-way handshake */
	MPTCP_MIB_MPCAPABLEACTIVEFALLBACK, /* Client-side fallback during 3-way handshake */
	MPTCP_MIB_RETRANSSEGS,		/* Segments retransmitted at the MPTCP-level */
	MPTCP_MIB_JOINNOTOKEN,		/* Received MP_JOIN but the token was not found */
	MPTCP_MIB_JOINSYNRX,		/* Received a SYN + MP_JOIN */
	MPTCP_MIB_JOINSYNACKRX,		/* Received a SYN/ACK + MP_JOIN */
	MPTCP_MIB_JOINSYNACKMAC,	/* HMAC was wrong on SYN/ACK + MP_JOIN */
	MPTCP_MIB_JOINACKRX,		/* Received an ACK + MP_JOIN */
	MPTCP_MIB_JOINACKMAC,		/* HMAC was wrong on ACK + MP_JOIN */
	MPTCP_MIB_DSSNOMATCH,		/* Received a new mapping that did not match the previous one */
	MPTCP_MIB_INFINITEMAPRX,	/* Received an infinite mapping */
	__MPTCP_MIB_MAX
};

#define LINUX_MIB_MPTCP_MAX	__MPTCP_MIB_MAX
struct mptcp_mib {
	unsigned long mibs[LINUX_MIB_MPTCP_MAX];
};

static inline void MPTCP_INC_STATS(struct net *net,
				   enum linux_mptcp_mib_field field)
{
	if (likely(net->mptcp_mib.mptcp_statistics))
		SNMP_INC_STATS(net->mptcp_mib.mptcp_statistics, field);
}

static inline void __MPTCP_INC_STATS(struct net *net,
				     enum linux_mptcp_mib_field field)
{
	if (likely(net->mptcp_mib.mptcp_statistics))
		__SNMP_INC_STATS(net->mptcp_mib.mptcp_statistics, field);
}

bool mptcp_mib_alloc(struct net *net);
