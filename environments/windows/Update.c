#define _WIN32_WINNT 0x0602
#include <windows.h>
#include <aclapi.h>
#include <sddl.h>
#include <bcrypt.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

void *sapiens_update_lease(const wchar_t *path, int exclusive) {
    HANDLE h = CreateFileW(path, GENERIC_READ | GENERIC_WRITE,
        FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    if (h == INVALID_HANDLE_VALUE) return NULL;
    OVERLAPPED range = {0};
    DWORD flags = (exclusive ? LOCKFILE_EXCLUSIVE_LOCK : 0) | LOCKFILE_FAIL_IMMEDIATELY;
    ULONGLONG deadline = GetTickCount64() + 15000;
    while (!LockFileEx(h, flags, 0, 1, 0, &range)) {
        if (GetLastError() != ERROR_LOCK_VIOLATION || exclusive == 2 || GetTickCount64() >= deadline) {
            CloseHandle(h); return NULL;
        }
        Sleep(50);
    }
    return h;
}
void sapiens_update_close(void *h) { CloseHandle(h); }
DWORD sapiens_update_downgrade(void *h) {
    OVERLAPPED range = {0};
    if (!LockFileEx(h, 0, 0, 1, 0, &range)) return GetLastError();
    if (!UnlockFileEx(h, 0, 1, 0, &range)) return GetLastError();
    return 0;
}
DWORD sapiens_update_publish(const wchar_t *path, const wchar_t *temporary, const BYTE *bytes, DWORD size, int replace) {
    HANDLE h = CreateFileW(temporary, GENERIC_WRITE, 0, NULL, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, NULL);
    if (h == INVALID_HANDLE_VALUE) return GetLastError();
    DWORD written = 0, error = 0;
    if (!WriteFile(h, bytes, size, &written, NULL) || written != size || !FlushFileBuffers(h)) error = GetLastError();
    if (!error && written != size) error = ERROR_WRITE_FAULT;
    if (!CloseHandle(h) && !error) error = GetLastError();
    if (!error && !MoveFileExW(temporary, path, MOVEFILE_WRITE_THROUGH | (replace ? MOVEFILE_REPLACE_EXISTING : 0))) error = GetLastError();
    if (error) DeleteFileW(temporary);
    return error;
}
static DWORD inspect(const wchar_t *path, BY_HANDLE_FILE_INFORMATION *info) {
    HANDLE h = CreateFileW(path, FILE_READ_ATTRIBUTES, FILE_SHARE_READ, NULL, OPEN_EXISTING,
        FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_BACKUP_SEMANTICS, NULL);
    if (h == INVALID_HANDLE_VALUE) return GetLastError();
    DWORD error = GetFileInformationByHandle(h, info) ? 0 : GetLastError();
    CloseHandle(h);
    if (!error && (info->dwFileAttributes & (FILE_ATTRIBUTE_REPARSE_POINT | FILE_ATTRIBUTE_ENCRYPTED |
        FILE_ATTRIBUTE_SPARSE_FILE | FILE_ATTRIBUTE_COMPRESSED | FILE_ATTRIBUTE_OFFLINE))) error = ERROR_NOT_SUPPORTED;
    if (!error && !(info->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) && info->nNumberOfLinks != 1) error = ERROR_NOT_SUPPORTED;
    return error;
}
DWORD sapiens_update_validate(const wchar_t *path) { BY_HANDLE_FILE_INFORMATION info; return inspect(path, &info); }
DWORD sapiens_update_open_root(const wchar_t *path, HANDLE *root, uint64_t *identity) {
    HANDLE h = CreateFileW(path, FILE_READ_ATTRIBUTES, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
        OPEN_EXISTING, FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_BACKUP_SEMANTICS, NULL);
    if (h == INVALID_HANDLE_VALUE) return GetLastError();
    BY_HANDLE_FILE_INFORMATION info;
    DWORD error = GetFileInformationByHandle(h, &info) ? 0 : GetLastError();
    if (!error && (!(info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) ||
        (info.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT))) error = ERROR_DIRECTORY;
    if (error) { CloseHandle(h); return error; }
    identity[0] = info.dwVolumeSerialNumber;
    identity[1] = ((uint64_t)info.nFileIndexHigh << 32) | info.nFileIndexLow;
    *root = h;
    return 0;
}
static DWORD security(const wchar_t *source, const wchar_t *destination) {
    PSID owner = NULL, group = NULL; PACL dacl = NULL; PSECURITY_DESCRIPTOR sd = NULL, dest = NULL;
    DWORD flags = OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION;
    DWORD error = GetNamedSecurityInfoW((wchar_t *)source, SE_FILE_OBJECT, flags, &owner, &group, &dacl, NULL, &sd);
    if (error) return error;
    SECURITY_DESCRIPTOR_CONTROL control; DWORD revision;
    if (!GetSecurityDescriptorControl(sd, &control, &revision)) { error = GetLastError(); goto done; }
    PSID destOwner = NULL, destGroup = NULL;
    error = GetNamedSecurityInfoW((wchar_t *)destination, SE_FILE_OBJECT, flags, &destOwner, &destGroup, NULL, NULL, &dest);
    if (error) goto done;
    flags = DACL_SECURITY_INFORMATION | ((control & SE_DACL_PROTECTED) ? PROTECTED_DACL_SECURITY_INFORMATION : UNPROTECTED_DACL_SECURITY_INFORMATION);
    if (!EqualSid(owner, destOwner)) flags |= OWNER_SECURITY_INFORMATION;
    if (!EqualSid(group, destGroup)) flags |= GROUP_SECURITY_INFORMATION;
    if (control & SE_DACL_AUTO_INHERITED)
        error = SetNamedSecurityInfoW((wchar_t *)destination, SE_FILE_OBJECT, flags, owner, group, dacl, NULL);
    else if (!SetFileSecurityW(destination, flags, sd)) error = GetLastError();
 done:
    if (dest) LocalFree(dest);
    LocalFree(sd);
    return error;
}
static DWORD attributes(const wchar_t *source, const wchar_t *destination, BOOL flush) {
    BY_HANDLE_FILE_INFORMATION info;
    DWORD error = inspect(source, &info);
    if (error) return error;
    DWORD attrs = GetFileAttributesW(destination);
    if (attrs == INVALID_FILE_ATTRIBUTES) return GetLastError();
    if (!SetFileAttributesW(destination, attrs & ~FILE_ATTRIBUTE_READONLY)) return GetLastError();
    HANDLE h = CreateFileW(destination, FILE_WRITE_ATTRIBUTES | (flush ? GENERIC_WRITE : 0), FILE_SHARE_READ,
        NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT, NULL);
    if (h == INVALID_HANDLE_VALUE) return GetLastError();
    if (!SetFileTime(h, &info.ftCreationTime, NULL, &info.ftLastWriteTime)) error = GetLastError();
    if (!error) error = security(source, destination);
    if (!SetFileAttributesW(destination, info.dwFileAttributes) && !error) error = GetLastError();
    if (!error && flush && !FlushFileBuffers(h)) error = GetLastError();
    if (!CloseHandle(h) && !error) error = GetLastError();
    return error;
}
DWORD sapiens_update_copy(const wchar_t *source, const wchar_t *destination) {
    BY_HANDLE_FILE_INFORMATION info;
    DWORD error = inspect(source, &info);
    if (error) return error;
    if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) return ERROR_DIRECTORY;
    if (!CopyFileW(source, destination, TRUE)) return GetLastError();
    return 0;
}
static wchar_t *streamPath(const wchar_t *path, const wchar_t *stream) {
    wchar_t *result = malloc((wcslen(path)+wcslen(stream)+1)*sizeof(wchar_t));
    if (result) { wcscpy(result,path); wcscat(result,stream); }
    return result;
}
DWORD sapiens_update_directory_metadata(const wchar_t *source, const wchar_t *destination) {
    BY_HANDLE_FILE_INFORMATION info;
    DWORD error = inspect(source,&info);
    if (!error) error = inspect(destination,&info);
    if (error) return error;
    WIN32_FIND_STREAM_DATA entry;
    HANDLE find = FindFirstStreamW(destination,FindStreamInfoStandard,&entry,0);
    if (find != INVALID_HANDLE_VALUE) {
        do {
            wchar_t *path = streamPath(destination,entry.cStreamName);
            if (!path) { error=ERROR_OUTOFMEMORY; break; }
            if (!DeleteFileW(path)) error=GetLastError();
            free(path);
            if (error) break;
        } while (FindNextStreamW(find,&entry));
        if (!error && GetLastError()!=ERROR_HANDLE_EOF) error=GetLastError();
        FindClose(find);
    } else if (GetLastError()!=ERROR_HANDLE_EOF) error=GetLastError();
    if (error) return error;
    find = FindFirstStreamW(source,FindStreamInfoStandard,&entry,0);
    if (find != INVALID_HANDLE_VALUE) {
        do {
            wchar_t *from=streamPath(source,entry.cStreamName), *to=streamPath(destination,entry.cStreamName);
            if (!from || !to) error=ERROR_OUTOFMEMORY;
            HANDLE input=INVALID_HANDLE_VALUE, output=INVALID_HANDLE_VALUE;
            if (!error) input=CreateFileW(from,GENERIC_READ,FILE_SHARE_READ,NULL,OPEN_EXISTING,0,NULL);
            if (!error && input==INVALID_HANDLE_VALUE) error=GetLastError();
            if (!error) output=CreateFileW(to,GENERIC_WRITE,0,NULL,CREATE_NEW,0,NULL);
            if (!error && output==INVALID_HANDLE_VALUE) error=GetLastError();
            BYTE buffer[65536]; DWORD count,written;
            while (!error) {
                if (!ReadFile(input,buffer,sizeof(buffer),&count,NULL)) { error=GetLastError(); break; }
                if (!count) break;
                if (!WriteFile(output,buffer,count,&written,NULL) || written!=count) { error=ERROR_WRITE_FAULT; break; }
            }
            if (!error && !FlushFileBuffers(output)) error=GetLastError();
            if (input!=INVALID_HANDLE_VALUE) CloseHandle(input);
            if (output!=INVALID_HANDLE_VALUE) CloseHandle(output);
            free(from); free(to);
            if (error) break;
        } while (FindNextStreamW(find,&entry));
        if (!error && GetLastError()!=ERROR_HANDLE_EOF) error=GetLastError();
        FindClose(find);
    } else if (GetLastError()!=ERROR_HANDLE_EOF) error=GetLastError();
    return error ? error : attributes(source,destination,FALSE);
}
DWORD sapiens_update_copy_metadata(const wchar_t *source, const wchar_t *destination) {
    DWORD kind = GetFileAttributesW(source);
    if (kind == INVALID_FILE_ATTRIBUTES) return GetLastError();
    return (kind & FILE_ATTRIBUTE_DIRECTORY) ? sapiens_update_directory_metadata(source,destination) : attributes(source,destination,TRUE);
}
static BOOL digest(const wchar_t *path, BYTE result[32]) {
    HANDLE file = CreateFileW(path, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
    if (file == INVALID_HANDLE_VALUE) return FALSE;
    BCRYPT_ALG_HANDLE alg = NULL; BCRYPT_HASH_HANDLE hash = NULL;
    BYTE bytes[65536]; DWORD count; BOOL ok = FALSE;
    if (BCryptOpenAlgorithmProvider(&alg, BCRYPT_SHA256_ALGORITHM, NULL, 0) < 0) goto done;
    if (BCryptCreateHash(alg, &hash, NULL, 0, NULL, 0, 0) < 0) goto done;
    for (;;) {
        if (!ReadFile(file, bytes, sizeof(bytes), &count, NULL)) goto done;
        if (!count) break;
        if (BCryptHashData(hash, bytes, count, 0) < 0) goto done;
    }
    ok = BCryptFinishHash(hash, result, 32, 0) >= 0;
 done:
    if (hash) BCryptDestroyHash(hash);
    if (alg) BCryptCloseAlgorithmProvider(alg, 0);
    CloseHandle(file); return ok;
}
DWORD sapiens_update_metadata(const wchar_t *path, BYTE *out, DWORD capacity) {
    BY_HANDLE_FILE_INFORMATION info; DWORD used = 0;
    if (inspect(path, &info)) return 0;
#define APPEND(p,n) do { DWORD length_ = (DWORD)(n); if (used + length_ > capacity) goto failed; memcpy(out+used,(p),length_); used += length_; } while(0)
    APPEND(&info.dwFileAttributes, sizeof(DWORD));
    APPEND(&info.ftCreationTime, sizeof(FILETIME)); APPEND(&info.ftLastWriteTime, sizeof(FILETIME));
    DWORD flags = OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION;
    PSECURITY_DESCRIPTOR sd = NULL; wchar_t *sddl = NULL;
    if (GetNamedSecurityInfoW((wchar_t *)path, SE_FILE_OBJECT, flags, NULL, NULL, NULL, NULL, &sd)) return 0;
    if (!ConvertSecurityDescriptorToStringSecurityDescriptorW(sd, SDDL_REVISION_1, flags, &sddl, NULL)) { LocalFree(sd); return 0; }
    DWORD length = (DWORD)((wcslen(sddl)+1)*sizeof(wchar_t));
    if (used + length > capacity) { LocalFree(sddl); LocalFree(sd); return 0; }
    memcpy(out+used,sddl,length); used += length; LocalFree(sddl); LocalFree(sd);
    {
        WIN32_FIND_STREAM_DATA data;
        HANDLE find = FindFirstStreamW(path, FindStreamInfoStandard, &data, 0);
        if (find == INVALID_HANDLE_VALUE) return GetLastError()==ERROR_HANDLE_EOF && (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) ? used : 0;
        do {
            wchar_t *stream = malloc((wcslen(path)+wcslen(data.cStreamName)+1)*sizeof(wchar_t));
            if (!stream) { FindClose(find); return 0; }
            wcscpy(stream,path); wcscat(stream,data.cStreamName);
            BYTE sha[32]; BOOL ok = digest(stream,sha); free(stream);
            length = (DWORD)((wcslen(data.cStreamName)+1)*sizeof(wchar_t));
            if (!ok || used+length+32>capacity) { FindClose(find); return 0; }
            memcpy(out+used,data.cStreamName,length); used+=length; memcpy(out+used,sha,32); used+=32;
        } while (FindNextStreamW(find,&data));
        DWORD error = GetLastError(); FindClose(find);
        if (error != ERROR_HANDLE_EOF) return 0;
    }
    return used;
 failed: return 0;
#undef APPEND
}
DWORD sapiens_update_job(void *job, int commit) {
    if (commit) {
        JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits = {0};
        if (!QueryInformationJobObject(job,JobObjectExtendedLimitInformation,&limits,sizeof(limits),NULL)) return GetLastError();
        limits.BasicLimitInformation.LimitFlags &= ~JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
        return SetInformationJobObject(job,JobObjectExtendedLimitInformation,&limits,sizeof(limits)) ? 0 : GetLastError();
    }
    if (!TerminateJobObject(job,1)) return GetLastError();
    JOBOBJECT_BASIC_ACCOUNTING_INFORMATION accounting;
    ULONGLONG deadline = GetTickCount64() + 30000;
    do {
        if (!QueryInformationJobObject(job,JobObjectBasicAccountingInformation,&accounting,sizeof(accounting),NULL)) return GetLastError();
        if (accounting.ActiveProcesses) {
            if (GetTickCount64() >= deadline) return ERROR_TIMEOUT;
            Sleep(10);
        }
    } while (accounting.ActiveProcesses);
    return 0;
}
