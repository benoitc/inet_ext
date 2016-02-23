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

## Contribute

For issues, comments or feedback please create an [issue](https://github.com/benoitc/inet_ext/issues).

