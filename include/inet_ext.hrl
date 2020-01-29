%% Ipv4 Special-Purpose Adress registry
%% https://www.iana.org/assignments/iana-ipv4-special-registry/iana-ipv4-special-registry.xhtml

-define(IPv4_LINKLOCAL_NERWORK, inet_cidr:parse("169.254.0.0/16")).

-define(IPv4_LOOPBACK_NETWORK, inet_cidr:parse("127.0.0.0/8")).

-define(IPv4_MULTICAST_NETWORK,inet_cidr:parse("224.0.0.0/4")).

-define(IPv4_PUBLIC_NETWORK, inet_cidr:parse("100.64.0.0/10")).

-define(IPv4_PRIVATE_NETWORKS, [inet_cidr:parse("0.0.0.0/8"),
                                inet_cidr:parse("10.0.0.0/8"),
                                inet_cidr:parse("127.0.0.0/8"),
                                inet_cidr:parse("168.254.0.0/16"),
                                inet_cidr:parse("172.16.0.0/12"),
                                inet_cidr:parse("192.0.0.0/29"),
                                inet_cidr:parse("192.0.0.170/31"),
                                inet_cidr:parse("192.0.2.0/24"),
                                inet_cidr:parse("192.168.0.0/16"),
                                inet_cidr:parse("198.18.0.0/15"),
                                inet_cidr:parse("203.0.113.0/24"),
                                inet_cidr:parse("240.0.0.0/4"),
                                inet_cidr:parse("255.255.255.255/32")]).

-define(IPv4_RESERVED_NETWORK, inet_cidr:parse("240.0.0.0/4")).

-define(IPv4_UNSPECIED_ADDR, {0, 0, 0, 0}).


%% IANA IPv6 Special-Purpose Address Registry
%% https://www.iana.org/assignments/iana-ipv6-special-registry/iana-ipv6-special-registry.xhtml

-define(IPv6_LINKLOCAL_NETWORK, inet_cidr:parse("fe80::/10")).

-define(IPv6_MULTICAST_NETWORK, inet_cidr:parse("ff00::/8")).

-define(IPv6_PRIVATE_NETWORKS, [inet_cidr:parse("::1/128"),
                                inet_cidr:parse("::/128"),
                                inet_cidr:parse("::ffff:0:0/96"),
                                inet_cidr:parse("100::/64"),
                                inet_cidr:parse("2001::/23"),
                                inet_cidr:parse("2001:2::/48"),
                                inet_cidr:parse("2001:db8::/32"),
                                inet_cidr:parse("2001:10::/28"),
                                inet_cidr:parse("fc00::/7"),
                                inet_cidr:parse("fe80::/10")]).

-define(IPv6_RESERVED_NETWORKS, [inet_cidr:parse("::/8"),
                                 inet_cidr:parse("100::/8"),
                                 inet_cidr:parse("200::/7"),
                                 inet_cidr:parse("400::/6"),
                                 inet_cidr:parse("800::/5"),
                                 inet_cidr:parse("1000::/4"),
                                 inet_cidr:parse("4000::/3"),
                                 inet_cidr:parse("6000::/3"),
                                 inet_cidr:parse("8000::/3"),
                                 inet_cidr:parse("A000::/3"),
                                 inet_cidr:parse("C000::/3"),
                                 inet_cidr:parse("E000::/4"),
                                 inet_cidr:parse("F000::/5"),
                                 inet_cidr:parse("F800::/6"),
                                 inet_cidr:parse("FE00::/9")]).

-define(IPv6_SITELOCAL_NETWORK, inet_cidr:parse("fec0::/10")).
