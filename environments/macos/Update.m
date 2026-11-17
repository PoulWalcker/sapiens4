#import <AppKit/AppKit.h>
#import <Security/Security.h>
#import <Sparkle/Sparkle.h>
#import <sys/stat.h>

// Sparkle owns package validation and replacement. This process lives outside the
// replaceable bundle and opens the selected root only after verifying the installed B.
@interface Update : NSObject <SPUUserDriver, SPUUpdaterDelegate>
@property(nonatomic,strong) NSURL *app, *state, *asset;
@property(nonatomic,copy) NSString *version, *root, *config, *oldVersion, *publicKey, *product;
@property(nonatomic) pid_t shellPID;
@property(nonatomic,strong) SPUUpdater *updater;
@property(nonatomic) BOOL installationPossible, awaitingCommit, returningToA;
@property(nonatomic,strong) NSTimer *timer;
@property(nonatomic,strong) NSDate *deadline;
@end

@implementation Update
- (NSDictionary *)installedInfo {
    return [NSDictionary dictionaryWithContentsOfURL:[self.app URLByAppendingPathComponent:@"Contents/Info.plist"]];
}
- (void)record:(NSString *)message {
    [message writeToURL:[self.state URLByAppendingPathComponent:@"outcome.txt"] atomically:YES
               encoding:NSUTF8StringEncoding error:NULL];
}
- (void)stopWith:(NSString *)message {
    [self record:message];
    exit(EXIT_FAILURE);
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
- (void)reopen:(BOOL)updated {
    NSError *error = nil;
    NSURL *marker = [self.state URLByAppendingPathComponent:@"same-format"];
    if (updated) {
        NSData *contract = [NSJSONSerialization dataWithJSONObject:@{@"contract":@"same-format-v1", @"version":self.version} options:0 error:&error];
        if (!contract || ![contract writeToURL:marker options:NSDataWritingAtomic error:&error])
            [self stopWith:error.localizedDescription];
    }
    if (rmdir([[self.state URLByAppendingPathComponent:@"pending"] fileSystemRepresentation]) != 0)
        [self stopWith:[NSString stringWithFormat:@"Cannot finish installation ownership: %s", strerror(errno)]];
    NSTask *task = [NSTask new];
    task.executableURL = [self.app URLByAppendingPathComponent:@"Contents/MacOS/Sapiens4"];
    NSMutableDictionary *env = [NSProcessInfo.processInfo.environment mutableCopy];
    if (self.root.length) env[@"SAPIENS_DATA_DIR"] = self.root;
    else [env removeObjectForKey:@"SAPIENS_DATA_DIR"];
    env[@"SAPIENS_CONFIG_DIR"] = self.config;
    task.environment = env;
    if (![task launchAndReturnError:&error]) [self stopWith:error.localizedDescription];
    exit(updated ? EXIT_SUCCESS : EXIT_FAILURE);
}
- (void)tick {
    if ([self.deadline timeIntervalSinceNow] <= 0) [self stopWith:@"The updater could not confirm completion. Repair this installation before reopening."];
    if (self.shell != nil) return;
    if (self.returningToA && [self validInstalledVersion:self.oldVersion]) [self reopen:NO];
    if (self.awaitingCommit && [self validInstalledVersion:self.version]) [self reopen:YES];
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
    if (state.stage != SPUUserUpdateStageNotDownloaded) self.installationPossible = YES;
    reply(SPUUserUpdateChoiceInstall);
}
- (void)showUpdateReleaseNotesWithDownloadData:(SPUDownloadData *)data {}
- (void)showUpdateReleaseNotesFailedToDownloadWithError:(NSError *)error {}
- (void)showUpdateNotFoundWithError:(NSError *)error acknowledgement:(void (^)(void))ack { ack(); }
- (void)showUpdaterError:(NSError *)error acknowledgement:(void (^)(void))ack { ack(); }
- (void)showDownloadInitiatedWithCancellation:(void (^)(void))cancel {}
- (void)showDownloadDidReceiveExpectedContentLength:(uint64_t)length {}
- (void)showDownloadDidReceiveDataOfLength:(uint64_t)length {}
- (void)showDownloadDidStartExtractingUpdate { self.installationPossible = YES; }
- (void)showExtractionReceivedProgress:(double)progress {}
- (void)showReadyToInstallAndRelaunch:(void (^)(SPUUserUpdateChoice))reply {
    self.awaitingCommit = YES;
    reply(SPUUserUpdateChoiceDismiss);
    [self finishAfterShellExits];
}
- (void)showInstallingUpdateWithApplicationTerminated:(BOOL)terminated retryTerminatingApplication:(void (^)(void))retry {}
- (void)showUpdateInstalledAndRelaunched:(BOOL)relaunched acknowledgement:(void (^)(void))ack { ack(); }
- (void)dismissUpdateInstallation {}
- (void)updater:(SPUUpdater *)updater didFinishUpdateCycleForUpdateCheck:(SPUUpdateCheck)check error:(NSError *)error {
    if (self.awaitingCommit) return;
    // An already-terminated host may be installed without the ready-to-relaunch callback.
    if (!error && self.installationPossible && !self.shell) {
        self.awaitingCommit = YES;
        self.deadline = [NSDate dateWithTimeIntervalSinceNow:120];
        [self tick];
        return;
    }
    NSString *message = error.localizedDescription ?: @"Update check finished without installing the selected version.";
    if (self.installationPossible) [self stopWith:message];
    self.returningToA = YES;
    [self record:message];
    [self finishAfterShellExits];
}
@end

int main(int argc, const char *argv[]) {
    @autoreleasepool {
        if (argc != 8) return EXIT_FAILURE;
        NSArray<NSString *> *args = NSProcessInfo.processInfo.arguments;
        Update *driver = [Update new];
        driver.app = [[NSURL fileURLWithPath:args[1] isDirectory:YES] URLByResolvingSymlinksInPath];
        driver.state = [[NSURL fileURLWithPath:args[2] isDirectory:YES] URLByResolvingSymlinksInPath];
        NSString *expected = [NSString stringWithFormat:@".%@.update", driver.app.lastPathComponent];
        if (![driver.state.lastPathComponent isEqual:expected] ||
            ![driver.state.URLByDeletingLastPathComponent.path isEqual:driver.app.URLByDeletingLastPathComponent.path]) return EXIT_FAILURE;
        BOOL directory = NO;
        if (![[NSFileManager defaultManager] fileExistsAtPath:[driver.state.path stringByAppendingPathComponent:@"pending"] isDirectory:&directory] || !directory) return EXIT_FAILURE;
        driver.version = args[3]; driver.asset = [NSURL URLWithString:args[4]];
        driver.root = args[5]; driver.config = args[6]; driver.shellPID = args[7].intValue;
        if (driver.shellPID <= 0 || ![driver.asset.scheme isEqual:@"https"]) return EXIT_FAILURE;
        [NSApplication sharedApplication];
        [NSApp setActivationPolicy:NSApplicationActivationPolicyProhibited];
        NSError *error = nil;
        if (![driver start:&error]) {
            [driver record:error.localizedDescription];
            driver.returningToA = YES;
            driver.timer = [NSTimer scheduledTimerWithTimeInterval:0.5 repeats:YES block:^(NSTimer *timer) { [driver tick]; }];
            [driver finishAfterShellExits];
        }
        [NSApp run];
    }
    return EXIT_FAILURE;
}
