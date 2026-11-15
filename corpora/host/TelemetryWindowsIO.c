/* Windows 8+ cancellable DNS. All socket IO stays nonblocking in Haskell. */
#if !defined(_WIN32_WINNT) || _WIN32_WINNT < 0x0602
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0602
#endif
#include <winsock2.h>
#include <ws2tcpip.h>
#include <windows.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef INT (WSAAPI *TelemetryDNSCancel)(LPHANDLE);
typedef INT (WSAAPI *TelemetryDNSResult)(LPOVERLAPPED);

typedef struct {
    OVERLAPPED overlapped;
    HANDLE event;
    HANDLE cancellation;
    PADDRINFOEXW addresses;
    int pending;
    int status;
    TelemetryDNSCancel cancel;
    TelemetryDNSResult result;
} TelemetryDNS;

void *sapiens_telemetry_dns_start(const wchar_t *host, const wchar_t *service) {
    TelemetryDNS *query = calloc(1, sizeof(*query));
    ADDRINFOEXW hints;
    HMODULE winsock;
    if (!query) return NULL;
    /* Some MinGW headers/import libraries omit the Windows 8 declarations. */
    winsock = GetModuleHandleW(L"ws2_32.dll");
    query->cancel = (TelemetryDNSCancel)GetProcAddress(winsock, "GetAddrInfoExCancel");
    query->result = (TelemetryDNSResult)GetProcAddress(winsock, "GetAddrInfoExOverlappedResult");
    if (!query->cancel || !query->result) { free(query); return NULL; }
    query->event = CreateEventW(NULL, TRUE, FALSE, NULL);
    if (!query->event) { free(query); return NULL; }
    query->overlapped.hEvent = query->event;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = IPPROTO_TCP;
    hints.ai_flags = AI_NUMERICSERV;
    query->status = GetAddrInfoExW(host, service, NS_ALL, NULL, &hints,
        &query->addresses, NULL, &query->overlapped, NULL, &query->cancellation);
    query->pending = query->status == WSA_IO_PENDING;
    return query;
}

/* 0: pending; 1: completed successfully; -1: failed. */
int sapiens_telemetry_dns_poll(void *value) {
    TelemetryDNS *query = value;
    if (query->pending) {
        if (WaitForSingleObject(query->event, 0) == WAIT_TIMEOUT) return 0;
        query->status = query->result(&query->overlapped);
        query->pending = 0;
    }
    return query->status == 0 ? 1 : -1;
}

int sapiens_telemetry_dns_address(void *value, unsigned int index, void *buffer) {
    TelemetryDNS *query = value;
    PADDRINFOEXW address = query->addresses;
    while (address && index--) address = address->ai_next;
    if (!address) return 0;
    if (address->ai_addrlen > sizeof(SOCKADDR_STORAGE)) return -1;
    memset(buffer, 0, sizeof(SOCKADDR_STORAGE));
    memcpy(buffer, address->ai_addr, address->ai_addrlen);
    return 1;
}

void sapiens_telemetry_dns_free(void *value) {
    TelemetryDNS *query = value;
    if (query->pending) {
        query->cancel(&query->cancellation);
        /* The provider must finish before OVERLAPPED/results can be freed. */
        WaitForSingleObject(query->event, INFINITE);
    }
    if (query->addresses) FreeAddrInfoExW(query->addresses);
    CloseHandle(query->event);
    free(query);
}

/* Capture WSAGetLastError before returning to a movable Haskell thread. */
int sapiens_telemetry_nonblocking(uintptr_t socket) {
    u_long enabled = 1;
    return ioctlsocket((SOCKET)socket, FIONBIO, &enabled) == 0 ? 0 : -WSAGetLastError();
}

int sapiens_telemetry_connect(uintptr_t socket, const void *address, int length) {
    return connect((SOCKET)socket, address, length) == 0 ? 0 : -WSAGetLastError();
}

int sapiens_telemetry_recv(uintptr_t socket, void *buffer, int length) {
    int received = recv((SOCKET)socket, buffer, length, 0);
    return received == SOCKET_ERROR ? -WSAGetLastError() : received;
}

int sapiens_telemetry_send(uintptr_t socket, const void *buffer, int length) {
    int sent = send((SOCKET)socket, buffer, length, 0);
    return sent == SOCKET_ERROR ? -WSAGetLastError() : sent;
}

int sapiens_telemetry_write_ready(uintptr_t socket) {
    fd_set writable, failed;
    struct timeval immediate = {0, 0};
    int result;
    FD_ZERO(&writable);
    FD_ZERO(&failed);
    FD_SET((SOCKET)socket, &writable);
    FD_SET((SOCKET)socket, &failed);
    /* Winsock reports failed nonblocking connects in the exception set. */
    result = select(0, NULL, &writable, &failed, &immediate);
    if (result == SOCKET_ERROR) return -WSAGetLastError();
    if (FD_ISSET((SOCKET)socket, &failed)) return -WSAECONNREFUSED;
    return FD_ISSET((SOCKET)socket, &writable) ? 1 : 0;
}
