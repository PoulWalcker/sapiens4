#import <AppKit/AppKit.h>
#import <Security/Security.h>
#import <Sparkle/Sparkle.h>
#import <sys/stat.h>
#import <signal.h>
#import <fcntl.h>
#import <poll.h>
#import <CommonCrypto/CommonDigest.h>
#import <sys/sysctl.h>
#import <sys/file.h>

// Sparkle owns package validation and replacement. This process lives outside the
// replaceable bundle and opens the selected root only after verifying the installed B.
typedef NS_ENUM(NSUInteger, Phase) { Downloading, Preserving, Installing };

@interface Update : NSObject <SPUUserDriver, SPUUpdaterDelegate>
@property(nonatomic,strong) NSURL *app, *state, *asset;
@property(nonatomic,copy) NSString *version, *root, *config, *oldVersion, *publicKey, *product;
@property(nonatomic) pid_t shellPID;
@property(nonatomic,strong) SPUUpdater *updater;
@property(nonatomic) Phase phase;
@property(nonatomic,strong) NSTimer *timer;
@property(nonatomic,strong) NSDate *deadline;
@property(nonatomic,strong) NSMutableDictionary *request;
@end

@implementation Update
- (NSDictionary *)installedInfo {
    return [NSDictionary dictionaryWithContentsOfURL:[self.app URLByAppendingPathComponent:@"Contents/Info.plist"]];
}
- (void)record:(NSString *)message {
    [message writeToURL:[self.state URLByAppendingPathComponent:@"outcome.txt"] atomically:YES
               encoding:NSUTF8StringEncoding error:NULL];
}
- (NSURL *)attemptDirectory { return NSBundle.mainBundle.bundleURL.URLByDeletingLastPathComponent; }
- (void)flush:(NSArray<NSURL *> *)paths {
    for (NSURL *path in paths) {
        int fd = open(path.fileSystemRepresentation, O_RDONLY);
        if (fd < 0) [self stopWith:@"Cannot open the update recovery files for flushing."];
        int status = fsync(fd);
        close(fd);
        if (status != 0) [self stopWith:@"Cannot flush the update recovery files to disk."];
    }
}
- (BOOL)releaseReservation {
    NSURL *pending = [self.state URLByAppendingPathComponent:@"pending"];
    NSString *attempt = [NSString stringWithContentsOfURL:pending encoding:NSUTF8StringEncoding error:NULL];
    return [attempt isEqual:self.attemptDirectory.lastPathComponent] && unlink(pending.fileSystemRepresentation) == 0;
}
- (void)stopWith:(NSString *)message {
    [self record:message];
    if (self.phase != Installing) [self releaseReservation];
    exit(EXIT_FAILURE);
}
- (NSNumber *)boot {
    struct timeval boot;
    size_t size = sizeof(boot);
    if (sysctlbyname("kern.boottime", &boot, &size, NULL, 0) != 0)
        [self stopWith:@"Cannot determine whether an interrupted installer is still running."];
    return @(boot.tv_sec);
}
- (void)repair:(BOOL)previous {
    self.phase = Installing; // Failure must retain the recovery gate and both archives.
    self.product = @"com.sapiens4.desktop";
    self.oldVersion = self.request[@"oldVersion"];
    self.publicKey = self.request[@"publicKey"];
    if (!self.oldVersion.length || !self.publicKey.length) [self stopWith:@"The recovery request is incomplete."];
    for (NSString *name in @[@"admission.lock", @"use.lock"]) {
        int fd = open([self.state URLByAppendingPathComponent:name].fileSystemRepresentation, O_RDWR | O_CLOEXEC);
        if (fd < 0 || flock(fd, LOCK_EX | LOCK_NB) != 0)
            [self stopWith:@"Close every Sapiens instance from this installation before recovery."];
        // The process owns these leases until exit, including during Finder replacement.
    }
    NSFileManager *files = NSFileManager.defaultManager;
    NSURL *pending = [self.state URLByAppendingPathComponent:@"pending"];
    BOOL exists = [files fileExistsAtPath:pending.path];
    if (exists) {
        NSString *attempt = [NSString stringWithContentsOfURL:pending encoding:NSUTF8StringEncoding error:NULL];
        if (![attempt isEqual:self.attemptDirectory.lastPathComponent])
            [self stopWith:@"A different update owns this installation."];
        // Sparkle's installer can outlive its controller. A reboot is the smallest
        // reliable boundary for an interrupted native replacement, without a supervisor.
        if ([self.request[@"boot"] isEqual:[self boot]])
            [self stopWith:@"Restart macOS, then run this Repair.command again. An earlier installer may still be running."];
    }
    NSURL *marker = [self.state URLByAppendingPathComponent:@"update.json"];
    NSData *bytes = [NSData dataWithContentsOfURL:marker];
    NSDictionary *current = bytes ? [NSJSONSerialization JSONObjectWithData:bytes options:0 error:NULL] : nil;
    if (!exists && bytes && (![current isKindOfClass:NSDictionary.class] || ![current[@"attempt"] isEqual:self.attemptDirectory.lastPathComponent]))
        [self stopWith:@"A newer update owns this installation. Use its recovery command."];
    NSString *target = previous ? self.oldVersion : self.version;
    NSURL *archive = [self.attemptDirectory URLByAppendingPathComponent:previous ? @"previous.zip" : @"installed.zip"];
    if (![self validInstalledVersion:target] && ![files fileExistsAtPath:archive.path])
        [self stopWith:@"No complete copy of B was preserved. Run Repair.command --previous to recover A; no data will be restored automatically."];
    NSError *error = nil;
    if (!exists) {
        if (![self.attemptDirectory.lastPathComponent writeToURL:pending atomically:YES encoding:NSUTF8StringEncoding error:&error]) [self stopWith:error.localizedDescription];
        [self flush:@[pending,self.state]];
    }
    self.request[@"boot"] = [self boot];
    NSURL *request = [self.attemptDirectory URLByAppendingPathComponent:@"request.json"];
    bytes = [NSJSONSerialization dataWithJSONObject:self.request options:0 error:&error];
    if (!bytes || ![bytes writeToURL:request options:NSDataWritingAtomic error:&error]) [self stopWith:error.localizedDescription];
    [self flush:@[request,self.attemptDirectory]];
    if (![self validInstalledVersion:target]) {
        NSURL *stage = [self.attemptDirectory URLByAppendingPathComponent:[@"repair-" stringByAppendingString:NSUUID.UUID.UUIDString]];
        NSTask *extract = [NSTask new];
        extract.executableURL = [NSURL fileURLWithPath:@"/usr/bin/ditto"];
        extract.arguments = @[@"-x",@"-k",archive.path,stage.path];
        if (![extract launchAndReturnError:&error]) [self stopWith:error.localizedDescription];
        [extract waitUntilExit];
        if (extract.terminationStatus != 0) [self stopWith:@"Cannot extract the preserved application."];
        NSURL *destination = self.app;
        self.app = [stage URLByAppendingPathComponent:destination.lastPathComponent];
        if (![self validInstalledVersion:target]) [self stopWith:@"The preserved application failed signature verification."];
        [NSWorkspace.sharedWorkspace activateFileViewerSelectingURLs:@[self.app]];
        printf("In Finder, copy %s to %s and choose Replace. Finder handles any required authorization.\n",
               self.app.path.UTF8String, destination.URLByDeletingLastPathComponent.path.UTF8String);
        self.app = destination;
        while (![self validInstalledVersion:target]) {
            printf("Press Return after Finder finishes, or close this window to cancel.\n"); fflush(stdout);
            char line[16];
            if (!fgets(line, sizeof(line), stdin)) [self stopWith:@"Recovery is unfinished. The application archives and data remain available."];
        }
    }
    if (!previous) [self reopen];
    if ([files fileExistsAtPath:marker.path]) {
        NSURL *saved = [self.attemptDirectory URLByAppendingPathComponent:@"restored-update.json"];
        if (rename(marker.fileSystemRepresentation, saved.fileSystemRepresentation) != 0) [self stopWith:@"Cannot finish application recovery."];
        [self flush:@[self.attemptDirectory,self.state]];
    }
    if (![self releaseReservation]) [self stopWith:@"Cannot finish installation ownership."];
    [self flush:@[self.state]];
    puts("Previous application restored. Explicitly restore the corresponding data before opening it; newer data must be preserved first.");
}
- (BOOL)validInstalledVersion:(NSString *)version {
    NSDictionary *info = self.installedInfo;
    if (![info[@"CFBundleIdentifier"] isEqual:self.product] ||
        ![info[@"CFBundleVersion"] isEqual:version] ||
        ![info[@"SUPublicEDKey"] isEqual:self.publicKey]) return NO;
    SecStaticCodeRef code = NULL;
    OSStatus status = SecStaticCodeCreateWithPath((__bridge CFURLRef)self.app, kSecCSDefaultFlags, &code);
    if (status != errSecSuccess) return NO;
    status = SecStaticCodeCheckValidity(code, kSecCSStrictValidate | kSecCSCheckNestedCode, NULL);
    CFRelease(code);
    return status == errSecSuccess;
}
- (NSRunningApplication *)shell {
    NSRunningApplication *p = [NSRunningApplication runningApplicationWithProcessIdentifier:self.shellPID];
    NSURL *expected = [self.app URLByAppendingPathComponent:@"Contents/MacOS/Sapiens4"];
    return [p.executableURL.URLByResolvingSymlinksInPath.path isEqual:expected.URLByResolvingSymlinksInPath.path] ? p : nil;
}
- (void)reopen {
    [self.timer invalidate];
    NSError *error = nil;
    NSURL *marker = [self.state URLByAppendingPathComponent:@"update.json"];
    {
        NSURL *metadata = [self.app URLByAppendingPathComponent:@"Contents/Resources/runtime/desktop-update.json"];
        NSData *bytes = [NSData dataWithContentsOfURL:metadata options:0 error:&error];
        id release = bytes ? [NSJSONSerialization JSONObjectWithData:bytes options:0 error:&error] : nil;
        if (![release isKindOfClass:NSDictionary.class] ||
            ![release[@"version"] isEqual:self.version]) [self stopWith:@"Installed release does not match the selected release."];
        [self archiveApplication:@"installed.zip"];
        NSData *record = [NSJSONSerialization dataWithJSONObject:@{@"attempt":self.attemptDirectory.lastPathComponent, @"version":self.version, @"status":@"installed"} options:0 error:&error];
        if (!record || ![record writeToURL:marker options:NSDataWritingAtomic error:&error])
            [self stopWith:error.localizedDescription];
        if (chmod(marker.fileSystemRepresentation, 0644) != 0) [self stopWith:@"Cannot publish the installation state."];
        [self flush:@[marker,self.state]];
    }
    if (![self releaseReservation])
        [self stopWith:[NSString stringWithFormat:@"Cannot finish installation ownership: %s", strerror(errno)]];
    [self flush:@[self.state]];
    NSTask *task = [NSTask new];
    task.executableURL = [self.app URLByAppendingPathComponent:@"Contents/MacOS/Sapiens4"];
    NSMutableDictionary *env = [NSProcessInfo.processInfo.environment mutableCopy];
    if (self.root.length) env[@"SAPIENS_DATA_DIR"] = self.root;
    else [env removeObjectForKey:@"SAPIENS_DATA_DIR"];
    env[@"SAPIENS_CONFIG_DIR"] = self.config;
    task.environment = env;
    if (![task launchAndReturnError:&error]) [self stopWith:error.localizedDescription];
    exit(EXIT_SUCCESS);
}
- (void)tick {
    if ([self.deadline timeIntervalSinceNow] <= 0) [self stopWith:@"The updater could not confirm completion. Repair this installation before reopening."];
    if (self.phase == Downloading && self.shell == nil)
        [self stopWith:@"The application closed before installation. The previous version is available."];
    if (self.shell != nil) return;
    if (self.phase == Installing && [self validInstalledVersion:self.version]) [self reopen];
}
- (void)finishAfterShellExits {
    // Dismiss at Sparkle's ready callback requests install-on-exit. Only the
    // explicit button can reach this helper; Sparkle will not relaunch the app.
    NSRunningApplication *shell = self.shell;
    if (shell && ![shell terminate]) [self stopWith:@"Close Sapiens to finish the update."];
    self.deadline = [NSDate dateWithTimeIntervalSinceNow:120];
    [self tick];
}
- (BOOL)start:(NSError **)error {
    NSDictionary *info = self.installedInfo;
    self.product = info[@"CFBundleIdentifier"];
    self.oldVersion = info[@"CFBundleVersion"];
    self.publicKey = info[@"SUPublicEDKey"];
    if (![self.product isEqual:@"com.sapiens4.desktop"] || !self.publicKey.length ||
        ![self validInstalledVersion:self.oldVersion]) [self stopWith:@"Cannot verify the original installed application."];
    self.request[@"oldVersion"] = self.oldVersion;
    self.request[@"publicKey"] = self.publicKey;
    self.request[@"boot"] = [self boot];
    NSURL *request = [self.attemptDirectory URLByAppendingPathComponent:@"request.json"];
    NSData *saved = [NSJSONSerialization dataWithJSONObject:self.request options:0 error:error];
    if (!saved || ![saved writeToURL:request options:NSDataWritingAtomic error:error]) return NO;
    NSURL *repair = [self.attemptDirectory URLByAppendingPathComponent:@"Repair.command"];
    NSString *script = @"#!/bin/sh\nexec \"$(dirname \"$0\")/Update.app/Contents/MacOS/Update\" \"${1:---repair}\"\n";
    if (![script writeToURL:repair atomically:YES encoding:NSUTF8StringEncoding error:error] ||
        chmod(repair.fileSystemRepresentation, 0700) != 0) return NO;
    [self flush:@[request,repair,self.attemptDirectory]];
    NSBundle *bundle = [NSBundle bundleWithURL:self.app];
    self.updater = [[SPUUpdater alloc] initWithHostBundle:bundle applicationBundle:bundle userDriver:self delegate:self];
    if (![self.updater startUpdater:error]) return NO;
    self.deadline = [NSDate dateWithTimeIntervalSinceNow:3600];
    self.timer = [NSTimer scheduledTimerWithTimeInterval:0.5 repeats:YES block:^(NSTimer *timer) { [self tick]; }];
    [self.updater checkForUpdates];
    return YES;
}
- (BOOL)updater:(SPUUpdater *)updater shouldProceedWithUpdate:(SUAppcastItem *)item updateCheck:(SPUUpdateCheck)check error:(NSError **)error {
    BOOL accepted = !item.deltaUpdate && !item.deltaUpdates.count && !item.informationOnlyUpdate &&
        [item.installationType isEqual:@"application"] && [item.versionString isEqual:self.version] &&
        [item.fileURL.absoluteString isEqual:self.asset.absoluteString];
    if (!accepted && error) *error = [NSError errorWithDomain:@"SapiensUpdate" code:1 userInfo:@{NSLocalizedDescriptionKey:@"The offered release changed. Check for updates again."}];
    return accepted;
}
- (BOOL)updater:(SPUUpdater *)updater shouldDownloadReleaseNotesForUpdate:(SUAppcastItem *)item { return NO; }
- (void)showUpdatePermissionRequest:(SPUUpdatePermissionRequest *)request reply:(void (^)(SUUpdatePermissionResponse *))reply {
    reply([[SUUpdatePermissionResponse alloc] initWithAutomaticUpdateChecks:NO automaticUpdateDownloading:@NO sendSystemProfile:NO]);
}
- (void)showUserInitiatedUpdateCheckWithCancellation:(void (^)(void))cancel {}
- (void)showUpdateFoundWithAppcastItem:(SUAppcastItem *)item state:(SPUUserUpdateState *)state reply:(void (^)(SPUUserUpdateChoice))reply {
    if (state.stage != SPUUserUpdateStageNotDownloaded)
        [self stopWith:@"An earlier staged update needs recovery before starting a new update."];
    reply(SPUUserUpdateChoiceInstall);
}
- (void)showUpdateReleaseNotesWithDownloadData:(SPUDownloadData *)data {}
- (void)showUpdateReleaseNotesFailedToDownloadWithError:(NSError *)error {}
- (void)showUpdateNotFoundWithError:(NSError *)error acknowledgement:(void (^)(void))ack { ack(); }
- (void)showUpdaterError:(NSError *)error acknowledgement:(void (^)(void))ack { ack(); }
- (void)showDownloadInitiatedWithCancellation:(void (^)(void))cancel {}
- (void)showDownloadDidReceiveExpectedContentLength:(uint64_t)length {}
- (void)showDownloadDidReceiveDataOfLength:(uint64_t)length {}
- (void)authorizeInstallation {
    if (self.phase != Downloading) return;
    if (puts("downloaded") == EOF || fflush(stdout) != 0)
        [self stopWith:@"The application closed before installation. The previous version is available."];
    // This callback blocks Sparkle before extraction. Its timeout must not depend
    // on the main run loop, which cannot run NSTimer while waiting here.
    NSDate *limit = [NSDate dateWithTimeIntervalSinceNow:60];
    char command[8];
    for (NSUInteger i = 0; i < sizeof(command); i++) {
        struct pollfd input = { STDIN_FILENO, POLLIN, 0 };
        int remaining = (int)([limit timeIntervalSinceNow] * 1000);
        if (remaining <= 0 || poll(&input, 1, remaining) <= 0 || read(STDIN_FILENO, &command[i], 1) != 1)
            [self stopWith:@"The update was cancelled before installation. The previous version is available."];
    }
    NSURL *pending = [self.state URLByAppendingPathComponent:@"pending"];
    NSString *attempt = [NSString stringWithContentsOfURL:pending encoding:NSUTF8StringEncoding error:NULL];
    if (memcmp(command, "install\n", sizeof(command)) != 0 || ![attempt isEqual:self.attemptDirectory.lastPathComponent])
        [self stopWith:@"Installation ownership was not granted."];
    self.phase = Preserving;
    [self archiveApplication:@"previous.zip"];
    self.phase = Installing;
}
- (void)archiveApplication:(NSString *)name {
    NSURL *archive = [self.attemptDirectory URLByAppendingPathComponent:name];
    NSTask *copy = [NSTask new];
    copy.executableURL = [NSURL fileURLWithPath:@"/usr/bin/ditto"];
    copy.arguments = @[@"-c",@"-k",@"--sequesterRsrc",@"--keepParent",self.app.path,archive.path];
    NSError *error = nil;
    if (![copy launchAndReturnError:&error]) [self stopWith:error.localizedDescription];
    [copy waitUntilExit];
    if (copy.terminationStatus != 0) [self stopWith:@"The previous application could not be preserved."];
    NSTask *verify = [NSTask new];
    verify.executableURL = [NSURL fileURLWithPath:@"/usr/bin/unzip"];
    verify.arguments = @[@"-tq",archive.path];
    verify.standardOutput = [NSFileHandle fileHandleWithNullDevice];
    if (![verify launchAndReturnError:&error]) [self stopWith:error.localizedDescription];
    [verify waitUntilExit];
    if (verify.terminationStatus != 0) [self stopWith:@"The preserved application archive could not be verified."];
    [self flush:@[archive,self.attemptDirectory]];
}
- (void)updater:(SPUUpdater *)updater willExtractUpdate:(SUAppcastItem *)item { [self authorizeInstallation]; }
- (void)showDownloadDidStartExtractingUpdate {}
- (void)showExtractionReceivedProgress:(double)progress {}
- (void)showReadyToInstallAndRelaunch:(void (^)(SPUUserUpdateChoice))reply {
    reply(SPUUserUpdateChoiceDismiss);
    [self finishAfterShellExits];
}
- (void)showInstallingUpdateWithApplicationTerminated:(BOOL)terminated retryTerminatingApplication:(void (^)(void))retry {}
- (void)showUpdateInstalledAndRelaunched:(BOOL)relaunched acknowledgement:(void (^)(void))ack { ack(); }
- (void)dismissUpdateInstallation {}
- (void)updater:(SPUUpdater *)updater didFinishUpdateCycleForUpdateCheck:(SPUUpdateCheck)check error:(NSError *)error {
    if (error) [self stopWith:error.localizedDescription];
    if (self.phase == Downloading) [self stopWith:@"The selected update was not downloaded."];
    self.deadline = [NSDate dateWithTimeIntervalSinceNow:120];
    [self tick];
}
@end

int main(int argc, const char *argv[]) {
    @autoreleasepool {
        if (argc > 2) return EXIT_FAILURE;
        NSArray<NSString *> *args = NSProcessInfo.processInfo.arguments;
        Update *driver = [Update new];
        NSData *request = [NSData dataWithContentsOfURL:[driver.attemptDirectory URLByAppendingPathComponent:@"request.json"]];
        driver.request = request ? [NSJSONSerialization JSONObjectWithData:request options:NSJSONReadingMutableContainers error:NULL] : nil;
        if (![driver.request isKindOfClass:NSMutableDictionary.class]) return EXIT_FAILURE;
        driver.app = [[NSURL fileURLWithPath:driver.request[@"app"] isDirectory:YES] URLByResolvingSymlinksInPath];
        driver.state = [[NSURL fileURLWithPath:driver.request[@"state"] isDirectory:YES] URLByResolvingSymlinksInPath];
        NSString *expected = [driver.app.URLByDeletingLastPathComponent.path stringByAppendingPathComponent:[NSString stringWithFormat:@".%@.update", driver.app.lastPathComponent]];
        if ([driver.app.path hasPrefix:@"/Applications/"]) {
            NSData *path = [driver.app.path dataUsingEncoding:NSUTF8StringEncoding];
            unsigned char digest[CC_SHA256_DIGEST_LENGTH];
            CC_SHA256(path.bytes, (CC_LONG)path.length, digest);
            NSMutableString *hash = [NSMutableString new];
            for (NSUInteger i = 0; i < sizeof(digest); i++) [hash appendFormat:@"%02x", digest[i]];
            expected = [@"/Users/Shared/.sapiens4-update-" stringByAppendingString:hash];
        }
        if (![driver.state.path isEqual:expected]) return EXIT_FAILURE;
        if (![driver.attemptDirectory.URLByDeletingLastPathComponent.path isEqual:driver.state.path]) return EXIT_FAILURE;
        driver.version = driver.request[@"version"]; driver.asset = [NSURL URLWithString:driver.request[@"asset"]];
        driver.root = driver.request[@"root"]; driver.config = driver.request[@"config"]; driver.shellPID = [driver.request[@"shellPid"] intValue];
        if (driver.shellPID <= 0 || ![driver.asset.scheme isEqual:@"https"]) return EXIT_FAILURE;
        signal(SIGPIPE, SIG_IGN);
        [NSApplication sharedApplication];
        [NSApp setActivationPolicy:NSApplicationActivationPolicyProhibited];
        if (argc == 2) {
            if (![args[1] isEqual:@"--repair"] && ![args[1] isEqual:@"--previous"]) return EXIT_FAILURE;
            [driver repair:[args[1] isEqual:@"--previous"]];
            return EXIT_SUCCESS;
        }
        NSError *error = nil;
        if (![driver start:&error]) [driver stopWith:error.localizedDescription];
        [NSApp run];
    }
    return EXIT_FAILURE;
}
