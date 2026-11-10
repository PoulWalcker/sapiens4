#include <gtk/gtk.h>
#include <glib-unix.h>
#include <signal.h>
#include <stdio.h>
#include "Browser.h"

typedef struct {
    GMainLoop *loop;
    GtkWindow *window;
    const char *url;
    int status;
} Desktop;

static gboolean close_window(GtkWidget *window, GdkEvent *event, Desktop *desktop) {
    (void)window; (void)event;
    g_main_loop_quit(desktop->loop);
    return FALSE;
}

static gboolean stop(gpointer data) {
    gtk_window_close(((Desktop *)data)->window);
    return G_SOURCE_CONTINUE;
}

static gboolean navigation(WebKitWebView *view, WebKitPolicyDecision *decision,
                           WebKitPolicyDecisionType type, Desktop *desktop) {
    (void)view;
    if (type == WEBKIT_POLICY_DECISION_TYPE_RESPONSE) return FALSE;
    WebKitNavigationAction *action = webkit_navigation_policy_decision_get_navigation_action(WEBKIT_NAVIGATION_POLICY_DECISION(decision));
    const char *uri = webkit_uri_request_get_uri(webkit_navigation_action_get_request(action));
    GUri *target = g_uri_parse(uri, G_URI_FLAGS_NONE, NULL);
    GUri *origin = g_uri_parse(desktop->url, G_URI_FLAGS_NONE, NULL);
    gboolean allowed = type == WEBKIT_POLICY_DECISION_TYPE_NAVIGATION_ACTION && target && origin &&
        !g_uri_get_userinfo(target) && !g_strcmp0(g_uri_get_scheme(target), g_uri_get_scheme(origin)) &&
        !g_strcmp0(g_uri_get_host(target), g_uri_get_host(origin)) && g_uri_get_port(target) == g_uri_get_port(origin) &&
        (!g_strcmp0(g_uri_get_path(target), g_uri_get_path(origin)) ||
         (!g_strcmp0(g_uri_get_path(origin), "/workspace/") && !g_strcmp0(g_uri_get_path(target), "/workspace")));
    if (target) g_uri_unref(target);
    if (origin) g_uri_unref(origin);
    if (allowed) webkit_policy_decision_use(decision);
    else webkit_policy_decision_ignore(decision);
    return TRUE;
}

static void loaded(WebKitWebView *view, WebKitLoadEvent event, Desktop *desktop) {
    (void)view;
    if (event == WEBKIT_LOAD_FINISHED && !desktop->status) {
        printf("READY %s\n", desktop->url);
        fflush(stdout);
    }
}

static gboolean failed(WebKitWebView *view, WebKitLoadEvent event, const char *uri,
                       GError *error, Desktop *desktop) {
    (void)view; (void)event; (void)uri;
    g_printerr("Workspace load failed: %s\n", error->message);
    desktop->status = 1;
    gtk_window_close(desktop->window);
    return TRUE;
}

int sapiens_linux_available(void) {
    return gtk_init_check(NULL, NULL);
}

static void terminated(WebKitWebView *view, WebKitWebProcessTerminationReason reason, Desktop *desktop) {
    (void)view;
    g_printerr("Workspace renderer terminated (%d). Restart Sapiens4 to recover.\n", reason);
    desktop->status = 1;
    gtk_window_close(desktop->window);
}

static gboolean key_pressed(GtkWidget *widget, GdkEventKey *event, WebKitWebView *view) {
    (void)widget;
    if (event->keyval == GDK_KEY_F5 || ((event->state & GDK_CONTROL_MASK) && event->keyval == GDK_KEY_r)) {
        webkit_web_view_reload(view);
        return TRUE;
    }
    return FALSE;
}

int sapiens_linux_run(const char *url, const char *data) {
    Desktop desktop = {g_main_loop_new(NULL, FALSE), GTK_WINDOW(gtk_window_new(GTK_WINDOW_TOPLEVEL)), url, 0};
    WebKitWebView *view = WEBKIT_WEB_VIEW(webkit_web_view_new());
    gtk_window_set_title(desktop.window, "Sapiens4");
    gtk_window_set_default_size(desktop.window, 1440, 960);
    GtkOverlay *overlay = GTK_OVERLAY(gtk_overlay_new());
    gtk_container_add(GTK_CONTAINER(overlay), GTK_WIDGET(view));
    gtk_container_add(GTK_CONTAINER(desktop.window), GTK_WIDGET(overlay));
    Browser *browser = sapiens_browser_new(view, overlay, url, data);
    g_signal_connect(desktop.window, "delete-event", G_CALLBACK(close_window), &desktop);
    g_signal_connect(view, "decide-policy", G_CALLBACK(navigation), &desktop);
    g_signal_connect(view, "load-changed", G_CALLBACK(loaded), &desktop);
    g_signal_connect(view, "load-failed", G_CALLBACK(failed), &desktop);
    g_signal_connect(view, "web-process-terminated", G_CALLBACK(terminated), &desktop);
    g_signal_connect(desktop.window, "key-press-event", G_CALLBACK(key_pressed), view);
    guint term = g_unix_signal_add(SIGTERM, stop, &desktop);
    guint interrupt = g_unix_signal_add(SIGINT, stop, &desktop);
    webkit_web_view_load_uri(view, url);
    gtk_widget_show_all(GTK_WIDGET(desktop.window));
    gtk_window_present(desktop.window);
    g_main_loop_run(desktop.loop);
    g_source_remove(term);
    g_source_remove(interrupt);
    sapiens_browser_free(browser);
    g_main_loop_unref(desktop.loop);
    return desktop.status;
}
