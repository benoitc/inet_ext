## inet_ext

inet extension, collections of convenient functions to use with inet


## Usage

### Get the lists of default gateway for each interfaces

```erlang

1> inet_ext:gateways().
[{"en0","192.168.1.254"},{"en1","192.168.1.254"}]
```

To get the gateway for a specific interface uses the `inet_ext:gateway_for/1` function.

### Get internal address for a gateway

```erlang
2> inet_ext:get_internal_address("192.168.1.254").
"192.168.1.22"
```

### Get all routes

```erlang
3> inet_ext:routes().
[{"lo0",
  {{65152,0,0,0,0,0,0,1},{65535,65535,65535,65535,0,0,0,0}}},
 {"lo0",{{127,0,0,1},{255,0,0,0}}},
 {"lo0",
  {{0,0,0,0,0,0,0,1},
   {65535,65535,65535,65535,65535,65535,65535,65535}}},
 {"en0",{{192,168,1,41},{255,255,255,0}}},
 {"en0",
  {{10753,3637,35380,28048,9323,2852,13025,48991},
   {65535,65535,65535,65535,0,0,0,0}}},
 {"en0",
  {{10753,3637,35380,28048,44679,41983,65072,60656},
   {65535,65535,65535,65535,0,0,0,0}}},
 {"en0",
  {{65152,0,0,0,44679,41983,65072,60656},
   {65535,65535,65535,65535,0,0,0,0}}},
 {"en1",{{192,168,1,22},{255,255,255,0}}},
 {"en1",
  {{10753,3637,35380,28048,24909,3247,60839,4143},
   {65535,65535,65535,65535,0,0,0,0}}}]
```

### IP and MAC address format conversions

|API|Description|
|-----:|:-----------|
|convert_mac/2|Convert MAC Address from -> to |
|convert_ip/2|Convert IP Address from -> to |

Conversions allowed are in between following types:
to_string | to_binstring | to_integer | to_binary | to_list | to_tuple

```erlang
1> inet_utils:convert_mac(to_binstring, <<1,2,3,4,5,6>>).
<<"01:02:03:04:05:06">>
2> inet_utils:convert_ip(to_binary, 16#01020304).
<<1,2,3,4>>
```

### Check address type

You can test an IP address to know if they it's a private,
global(public), loopback, reserved, unspecified, liknlocal or multicast
address by using the following functions:

    * `is_private_address/1`
    * `is_global_address/1`
    * `is_loopback_address/1`
    * `is_unspecified_address/1`
    * `is_reserved_address/1`
    * `is_linklocal_address/1`
    * `is_multicast_address/1`

These functions are using based on IANA [IPv4](https://www.iana.org/assignments/iana-ipv4-special-registry/iana-ipv4-special-registry.xhtml) & [IPv6](https://www.iana.org/assignments/iana-ipv6-special-registry/iana-ipv6-special-registry.xhtml
) special-purpose address registries.


```erlang
1> inet_ext:is_private_address("192.168.1.1").
true
```

## Contribute

For issues, comments or feedback please create an [issue](https://github.com/benoitc/inet_ext/issues).

