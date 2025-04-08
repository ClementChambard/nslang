//////////////////////////////////////////////
/// Networking standard Library for NSLang ///
//////////////////////////////////////////////

/// constants
///   Various networking constants
enum : i64 {
    // TODO:
    AF_INET = 2,
    SOCK_STREAM = 1,
    INADDR_ANY = 0,
    SHUT_RDWR = 2,
};

/// type sa_family_t
type sa_family_t = u16;

/// type in_port_t
type in_port_t = u16;

/// type in_addr_t
type in_addr_t = u32;

/// type InAddr
struct InAddr {
    s_addr: in_addr_t;
};

/// type SockAddr
struct SockAddr {
    sa_family: sa_family_t;
    sa_data: i8[14];
};

/// type SockAddrIn
struct SockAddrIn {
    sin_family: sa_family_t;
    sin_port: in_port_t;
    sin_addr: InAddr;
    sin_zero: i8[sizeof(SockAddr) - sizeof(in_port_t) - sizeof(InAddr) - sizeof(sa_family_t)];
};

/// function socket
lib fn socket(domain: i64, ty: i64, protocol: i64) -> i64;

/// function connect
lib fn connect(sockfd: i64, addr: SockAddr*, addrlen: i64) -> i64;

/// function accept
lib fn accept(sockfd: i64, addr: SockAddr*, addrlen: i64*) -> i64;

/// function shutdown
lib fn shutdown(sockfd: i64, how: i64) -> i64;

/// function bind
lib fn bind(sockfd: i64, addr: void*, addrlen: i64) -> i64;

/// function listen
lib fn listen(sockfd: i64, backlog: i64) -> i64;

/// function send
lib fn send(sockfd: i64, buf: void*, size: i64, flags: i64) -> i64;

/// function recv
lib fn recv(sockfd: i64, buf: void*, size: i64, flags: i64) -> i64;
