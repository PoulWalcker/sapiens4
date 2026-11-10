#pragma once
#include <webkit2/webkit2.h>

typedef struct Browser Browser;
Browser *sapiens_browser_new(WebKitWebView *shell, GtkOverlay *overlay, const char *origin, const char *data);
void sapiens_browser_free(Browser *browser);
