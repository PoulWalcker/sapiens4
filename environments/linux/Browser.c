#define _GNU_SOURCE
#include "Browser.h"
#include <errno.h>
#include <fcntl.h>
#include <glib/gstdio.h>
#include <stdio.h>
#include <json-glib/json-glib.h>
#include <math.h>

typedef struct { Browser *browser; char *owner; GtkFileChooserNative *dialog; } FilePick;

struct Browser {
    WebKitWebView *shell;
    GtkOverlay *overlay;
    GHashTable *tabs, *sessions;
    FilePick *pick;
    WebKitDownload *download;
    char *origin, *data;
};

typedef struct {
    Browser *browser;
    WebKitWebView *view;
    GtkFileChooserNative *save;
    char *save_path, *staging_dir;
    WebKitDownload *download;
    char *owner, *id, *sequence, *error, *requested;
    gboolean retained;
} Tab;

static const char *string(JsonObject *object, const char *name) {
    JsonNode *node = object ? json_object_get_member(object, name) : NULL;
    return node && json_node_get_value_type(node) == G_TYPE_STRING ? json_node_get_string(node) : "";
}

static JsonObject *object(JsonObject *parent, const char *name) {
    JsonNode *node = parent ? json_object_get_member(parent, name) : NULL;
    return node && JSON_NODE_HOLDS_OBJECT(node) ? json_node_get_object(node) : NULL;
}

static double number(JsonObject *parent, const char *name) {
    JsonNode *node = parent ? json_object_get_member(parent, name) : NULL;
    if (!node || (json_node_get_value_type(node) != G_TYPE_DOUBLE && json_node_get_value_type(node) != G_TYPE_INT64)) return 0;
    double value = json_node_get_double(node);
    return isfinite(value) ? value : 0;
}

static gboolean boolean(JsonObject *parent, const char *name) {
    JsonNode *node = parent ? json_object_get_member(parent, name) : NULL;
    return node && json_node_get_value_type(node) == G_TYPE_BOOLEAN && json_node_get_boolean(node);
}

static gboolean guest_allowed(Browser *browser, const char *address) {
    if (!g_strcmp0(address, "about:blank")) return TRUE;
    GUri *uri = g_uri_parse(address, G_URI_FLAGS_NONE, NULL);
    GUri *origin = g_uri_parse(browser->origin, G_URI_FLAGS_NONE, NULL);
    const char *scheme = uri ? g_uri_get_scheme(uri) : NULL;
    const char *host = uri ? g_uri_get_host(uri) : NULL;
    gboolean local = !host || !*host || !g_ascii_strcasecmp(host, "localhost") ||
        !g_strcmp0(host, "::1") || g_str_has_prefix(host, "127.");
    gboolean allowed = uri && !g_uri_get_userinfo(uri) &&
        ((!g_strcmp0(scheme, "file") && local) ||
         ((!g_strcmp0(scheme, "http") || !g_strcmp0(scheme, "https")) && host &&
          !(local && g_uri_get_port(uri) == g_uri_get_port(origin))));
    if (uri) g_uri_unref(uri);
    g_uri_unref(origin);
    return allowed;
}

static void emit(Browser *browser, JsonObject *value) {
    JsonNode *node = json_node_new(JSON_NODE_OBJECT);
    json_node_take_object(node, value);
    char *json = json_to_string(node, FALSE);
    char *script = g_strconcat("window.sapiensBrowserEvent?.(", json, ")", NULL);
    webkit_web_view_evaluate_javascript(browser->shell, script, -1, NULL, NULL, NULL, NULL, NULL);
    g_free(script);
    g_free(json);
    json_node_free(node);
}

static void report(Tab *tab) {
    if (!*tab->sequence) return;
    JsonObject *value = json_object_new();
    json_object_set_string_member(value, "owner", tab->owner);
    json_object_set_string_member(value, "id", tab->id);
    json_object_set_string_member(value, "seq", tab->sequence);
    const char *uri = webkit_web_view_get_uri(tab->view), *title = webkit_web_view_get_title(tab->view);
    json_object_set_string_member(value, "url", uri && *uri ? uri : tab->requested);
    json_object_set_string_member(value, "title", title ? title : "");
    json_object_set_string_member(value, "error", tab->error);
    json_object_set_boolean_member(value, "loading", webkit_web_view_is_loading(tab->view));
    json_object_set_boolean_member(value, "can_back", webkit_web_view_can_go_back(tab->view));
    json_object_set_boolean_member(value, "can_forward", webkit_web_view_can_go_forward(tab->view));
    emit(tab->browser, value);
}

static void observed(WebKitWebView *view, GParamSpec *property, Tab *tab) {
    (void)view; (void)property;
    report(tab);
}

static gboolean navigation(WebKitWebView *view, WebKitPolicyDecision *decision, WebKitPolicyDecisionType type, Tab *tab) {
    (void)view;
    if (type == WEBKIT_POLICY_DECISION_TYPE_RESPONSE) return FALSE;
    WebKitNavigationAction *action = webkit_navigation_policy_decision_get_navigation_action(WEBKIT_NAVIGATION_POLICY_DECISION(decision));
    const char *uri = webkit_uri_request_get_uri(webkit_navigation_action_get_request(action));
    gboolean allowed = guest_allowed(tab->browser, uri);
    if (allowed && type == WEBKIT_POLICY_DECISION_TYPE_NAVIGATION_ACTION) webkit_policy_decision_use(decision);
    else {
        webkit_policy_decision_ignore(decision);
        if (allowed) {
            JsonObject *value = json_object_new();
            json_object_set_string_member(value, "owner", tab->owner);
            json_object_set_string_member(value, "open", uri);
            emit(tab->browser, value);
        } else {
            g_free(tab->error);
            tab->error = g_strdup("Navigation blocked: unsupported destination");
            report(tab);
        }
    }
    return TRUE;
}

static gboolean failed(WebKitWebView *view, WebKitLoadEvent event, const char *uri, GError *error, Tab *tab) {
    (void)view; (void)event; (void)uri;
    // Ignoring a navigation can asynchronously interrupt its frame load. Keep
    // the reason reported by the policy decision and suppress the stock error page.
    if (g_error_matches(error, WEBKIT_POLICY_ERROR, WEBKIT_POLICY_ERROR_FRAME_LOAD_INTERRUPTED_BY_POLICY_CHANGE))
        return TRUE;
    if (!g_error_matches(error, WEBKIT_NETWORK_ERROR, WEBKIT_NETWORK_ERROR_CANCELLED)) {
        g_free(tab->error);
        tab->error = g_strdup(error->message);
        report(tab);
    }
    return FALSE;
}

static gboolean permission(WebKitWebView *view, WebKitPermissionRequest *request, gpointer data) {
    (void)view; (void)data;
    webkit_permission_request_deny(request);
    return TRUE;
}

static void terminated(WebKitWebView *view, WebKitWebProcessTerminationReason reason, Tab *tab) {
    (void)view; (void)reason;
    g_free(tab->error);
    tab->error = g_strdup("Browser renderer terminated. Reload to recover.");
    report(tab);
}

static void save_close(Tab *tab) {
    if (!tab->save) return;
    g_signal_handlers_disconnect_by_data(tab->save, tab);
    gtk_native_dialog_destroy(GTK_NATIVE_DIALOG(tab->save));
    g_clear_object(&tab->save);
}

static const char destination_exists[] = "The selected download destination already exists.";

static void download_error(Tab *tab, const char *message) {
    g_free(tab->error);
    tab->error = g_strdup(message);
    report(tab);
}

static void download_clear(Tab *tab) {
    save_close(tab);
    if (!tab->download) return;
    g_signal_handlers_disconnect_by_data(tab->download, tab);
    tab->browser->download = NULL;
    g_clear_object(&tab->download);
    g_clear_pointer(&tab->save_path, g_free);
    if (tab->staging_dir) {
        // WebKit may leave a partial file when a download fails or is cancelled.
        GDir *dir = g_dir_open(tab->staging_dir, 0, NULL);
        if (dir) {
            const char *name;
            while ((name = g_dir_read_name(dir))) {
                char *path = g_build_filename(tab->staging_dir, name, NULL);
                g_unlink(path);
                g_free(path);
            }
            g_dir_close(dir);
        }
        g_rmdir(tab->staging_dir);
        g_clear_pointer(&tab->staging_dir, g_free);
    }
}

static void download_finished(WebKitDownload *download, Tab *tab) {
    (void)download;
    if (tab->save_path) {
        char *payload = g_build_filename(tab->staging_dir, "payload", NULL);
        // The destination can appear after the chooser closes. Publish the
        // completed download atomically without replacing any existing entry.
        if (renameat2(AT_FDCWD, payload, AT_FDCWD, tab->save_path, RENAME_NOREPLACE) != 0)
            download_error(tab, errno == EEXIST ? destination_exists : g_strerror(errno));
        g_free(payload);
    }
    download_clear(tab);
}

static void download_failed(WebKitDownload *download, GError *error, Tab *tab) {
    (void)download;
    // WebKit emits finished after failed as well.
    g_clear_pointer(&tab->save_path, g_free);
    if (!g_error_matches(error, WEBKIT_DOWNLOAD_ERROR, WEBKIT_DOWNLOAD_ERROR_CANCELLED_BY_USER))
        download_error(tab, error->message);
}

static void saved(GtkNativeDialog *dialog, int response, Tab *tab) {
    char *path = response == GTK_RESPONSE_ACCEPT ? gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog)) : NULL;
    save_close(tab);
    if (!path) {
        webkit_download_cancel(tab->download);
        return;
    }
    if (g_file_test(path, G_FILE_TEST_EXISTS | G_FILE_TEST_IS_SYMLINK)) {
        download_error(tab, destination_exists);
        webkit_download_cancel(tab->download);
        g_free(path);
        return;
    }
    // WebKit can remove its destination while cleaning up a rejected download.
    // Give it only a private path on the selected destination's filesystem.
    char *parent = g_path_get_dirname(path);
    char *directory = g_build_filename(parent, ".sapiens-download-XXXXXX", NULL);
    g_free(parent);
    if (!g_mkdtemp(directory)) {
        download_error(tab, g_strerror(errno));
        webkit_download_cancel(tab->download);
        g_free(directory);
        g_free(path);
        return;
    }
    tab->staging_dir = directory;
    char *payload = g_build_filename(directory, "payload", NULL);
    GError *error = NULL;
    char *uri = g_filename_to_uri(payload, NULL, &error);
    g_free(payload);
    if (uri) {
        tab->save_path = path;
        webkit_download_set_destination(tab->download, uri);
        g_free(uri);
    } else {
        download_error(tab, error->message);
        g_error_free(error);
        webkit_download_cancel(tab->download);
        g_free(path);
    }
}

static gboolean download_destination(WebKitDownload *download, const char *suggested, Tab *tab) {
    (void)download;
    tab->save = gtk_file_chooser_native_new("Save download", GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(tab->view))),
        GTK_FILE_CHOOSER_ACTION_SAVE, "Save", "Cancel");
    char *name = g_path_get_basename(suggested && *suggested ? suggested : "download");
    gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(tab->save), name);
    gtk_file_chooser_set_local_only(GTK_FILE_CHOOSER(tab->save), TRUE);
    const char *directory = g_get_user_special_dir(G_USER_DIRECTORY_DOWNLOAD);
    if (directory) gtk_file_chooser_set_current_folder(GTK_FILE_CHOOSER(tab->save), directory);
    g_free(name);
    g_signal_connect(tab->save, "response", G_CALLBACK(saved), tab);
    gtk_native_dialog_show(GTK_NATIVE_DIALOG(tab->save));
    return TRUE;
}

static void download_started(WebKitWebContext *context, WebKitDownload *download, Browser *browser) {
    (void)context;
    WebKitWebView *view = webkit_download_get_web_view(download);
    GHashTableIter iterator;
    gpointer value;
    g_hash_table_iter_init(&iterator, browser->tabs);
    while (!browser->pick && !browser->download && g_hash_table_iter_next(&iterator, NULL, &value)) {
        Tab *tab = value;
        if (tab->view != view || !gtk_widget_get_mapped(GTK_WIDGET(view))) continue;
        tab->download = g_object_ref(download);
        browser->download = download;
        webkit_download_set_allow_overwrite(download, FALSE);
        g_signal_connect(download, "decide-destination", G_CALLBACK(download_destination), tab);
        g_signal_connect(download, "failed", G_CALLBACK(download_failed), tab);
        g_signal_connect(download, "finished", G_CALLBACK(download_finished), tab);
        return;
    }
    webkit_download_cancel(download);
}

static void tab_free(gpointer data) {
    Tab *tab = data;
    g_clear_pointer(&tab->save_path, g_free);
    if (tab->download) webkit_download_cancel(tab->download);
    download_clear(tab);
    g_signal_handlers_disconnect_by_data(tab->view, tab);
    webkit_web_view_stop_loading(tab->view);
    if (gtk_widget_get_parent(GTK_WIDGET(tab->view))) gtk_container_remove(GTK_CONTAINER(tab->browser->overlay), GTK_WIDGET(tab->view));
    g_object_unref(tab->view);
    g_free(tab->owner); g_free(tab->id); g_free(tab->sequence); g_free(tab->error); g_free(tab->requested); g_free(tab);
}

static Tab *tab_new(Browser *browser, const char *owner, const char *id) {
    WebKitWebContext *session = g_hash_table_lookup(browser->sessions, owner);
    if (!session) {
        char *digest = g_compute_checksum_for_string(G_CHECKSUM_SHA256, owner, -1);
        char *data = g_build_filename(browser->data, "browser", digest, "data", NULL);
        char *cache = g_build_filename(browser->data, "browser", digest, "cache", NULL);
        WebKitWebsiteDataManager *storage = webkit_website_data_manager_new("base-data-directory", data, "base-cache-directory", cache, NULL);
        g_mkdir_with_parents(data, 0700);
        char *cookies = g_build_filename(data, "cookies.sqlite", NULL);
        webkit_cookie_manager_set_persistent_storage(webkit_website_data_manager_get_cookie_manager(storage),
            cookies, WEBKIT_COOKIE_PERSISTENT_STORAGE_SQLITE);
        g_free(cookies);
        session = webkit_web_context_new_with_website_data_manager(storage);
        g_signal_connect(session, "download-started", G_CALLBACK(download_started), browser);
        g_object_unref(storage);
        g_hash_table_insert(browser->sessions, g_strdup(owner), session);
        g_free(digest); g_free(data); g_free(cache);
    }
    Tab *tab = g_new0(Tab, 1);
    tab->browser = browser;
    tab->owner = g_strdup(owner); tab->id = g_strdup(id); tab->sequence = g_strdup(""); tab->error = g_strdup("");
    tab->requested = g_strdup("about:blank");
    tab->view = g_object_new(WEBKIT_TYPE_WEB_VIEW, "web-context", session, NULL);
    g_object_ref_sink(tab->view);
    gtk_widget_set_visible(GTK_WIDGET(tab->view), FALSE);
    gtk_widget_set_halign(GTK_WIDGET(tab->view), GTK_ALIGN_START);
    gtk_widget_set_valign(GTK_WIDGET(tab->view), GTK_ALIGN_START);
    g_signal_connect(tab->view, "notify::title", G_CALLBACK(observed), tab);
    g_signal_connect(tab->view, "notify::uri", G_CALLBACK(observed), tab);
    g_signal_connect(tab->view, "notify::is-loading", G_CALLBACK(observed), tab);
    g_signal_connect(tab->view, "decide-policy", G_CALLBACK(navigation), tab);
    g_signal_connect(tab->view, "load-failed", G_CALLBACK(failed), tab);
    g_signal_connect(tab->view, "permission-request", G_CALLBACK(permission), tab);
    g_signal_connect(tab->view, "web-process-terminated", G_CALLBACK(terminated), tab);
    return tab;
}

static void apply(Tab *tab, JsonObject *spec, gboolean fresh) {
    double zoom = number(spec, "zoom");
    if (zoom >= 0.25 && zoom <= 5) webkit_web_view_set_zoom_level(tab->view, zoom);
    JsonObject *command = object(spec, "command");
    const char *sequence = string(command, "seq"), *action = string(command, "action");
    if (!fresh && (!*sequence || !g_strcmp0(sequence, tab->sequence))) return;
    g_free(tab->sequence); tab->sequence = g_strdup(sequence);
    g_free(tab->error); tab->error = g_strdup("");
    if (fresh || !g_strcmp0(action, "navigate")) {
        const char *uri = string(spec, "url");
        g_free(tab->requested); tab->requested = g_strdup(uri);
        if (guest_allowed(tab->browser, uri)) webkit_web_view_load_uri(tab->view, uri);
        else { g_free(tab->error); tab->error = g_strdup("Unsupported destination"); }
    } else if (!g_strcmp0(action, "reload")) webkit_web_view_reload(tab->view);
    else if (!g_strcmp0(action, "back")) webkit_web_view_go_back(tab->view);
    else if (!g_strcmp0(action, "forward")) webkit_web_view_go_forward(tab->view);
    report(tab);
}

static void synchronize(Browser *browser, JsonObject *message) {
    JsonObject *workspaces = object(message, "workspaces"), *rect = object(message, "rect");
    if (!workspaces) return;
    GHashTableIter iterator;
    gpointer value;
    g_hash_table_iter_init(&iterator, browser->tabs);
    while (g_hash_table_iter_next(&iterator, NULL, &value)) ((Tab *)value)->retained = FALSE;
    GList *owners = workspaces ? json_object_get_members(workspaces) : NULL;
    for (GList *item = owners; item; item = item->next) {
        const char *owner = item->data;
        JsonObject *workspace = object(workspaces, owner);
        JsonNode *node = workspace ? json_object_get_member(workspace, "tabs") : NULL;
        JsonArray *tabs = node && JSON_NODE_HOLDS_ARRAY(node) ? json_node_get_array(node) : NULL;
        for (guint i = 0; tabs && i < json_array_get_length(tabs) && i < 128; i++) {
            JsonNode *entry = json_array_get_element(tabs, i);
            if (!JSON_NODE_HOLDS_OBJECT(entry)) continue;
            JsonObject *spec = json_node_get_object(entry);
            const char *id = string(spec, "id");
            if (!*id) continue;
            char *key = g_strdup_printf("%zu:%s%s", strlen(owner), owner, id);
            Tab *tab = g_hash_table_lookup(browser->tabs, key);
            gboolean fresh = tab == NULL;
            gboolean shown = boolean(message, "visible") && !g_strcmp0(owner, string(message, "owner")) &&
                !g_strcmp0(id, string(message, "active"));
            if (!tab && shown && g_hash_table_size(browser->tabs) < 128) {
                tab = tab_new(browser, owner, id);
                g_hash_table_insert(browser->tabs, g_strdup(key), tab);
            }
            g_free(key);
            if (!tab) continue;
            tab->retained = TRUE;
            apply(tab, spec, fresh);
            if (shown) {
                if (!gtk_widget_get_parent(GTK_WIDGET(tab->view))) gtk_overlay_add_overlay(browser->overlay, GTK_WIDGET(tab->view));
                double scale = webkit_web_view_get_zoom_level(browser->shell);
                gtk_widget_set_margin_start(GTK_WIDGET(tab->view), CLAMP(number(rect, "x") * scale, 0, 20000));
                gtk_widget_set_margin_top(GTK_WIDGET(tab->view), CLAMP(number(rect, "y") * scale, 0, 20000));
                gtk_widget_set_size_request(GTK_WIDGET(tab->view), CLAMP(number(rect, "width") * scale, 1, 20000),
                    CLAMP(number(rect, "height") * scale, 1, 20000));
            }
            if (!shown && gtk_widget_has_focus(GTK_WIDGET(tab->view))) gtk_widget_grab_focus(GTK_WIDGET(browser->shell));
            gtk_widget_set_visible(GTK_WIDGET(tab->view), shown);
            // Hidden WebKit widgets remain in GTK's accessibility tree. Detach
            // inactive guests while retaining their page and session state.
            if (!shown && gtk_widget_get_parent(GTK_WIDGET(tab->view)))
                gtk_container_remove(GTK_CONTAINER(browser->overlay), GTK_WIDGET(tab->view));
        }
    }
    g_list_free(owners);
    g_hash_table_iter_init(&iterator, browser->tabs);
    while (g_hash_table_iter_next(&iterator, NULL, &value)) if (!((Tab *)value)->retained) g_hash_table_iter_remove(&iterator);
}

static void unload_idle(Browser *browser, JsonObject *message) {
    const char *owner = string(message, "owner"), *id = string(message, "id"), *path = string(message, "path");
    char *key = g_strdup_printf("%zu:%s%s", strlen(owner), owner, id);
    Tab *tab = g_hash_table_lookup(browser->tabs, key);
    if (tab && g_path_is_absolute(path) && !g_strcmp0(tab->sequence, string(message, "seq")) &&
        !*tab->error && !tab->download && !gtk_widget_get_visible(GTK_WIDGET(tab->view)) &&
        !webkit_web_view_is_loading(tab->view) && !webkit_web_view_can_go_back(tab->view) &&
        !webkit_web_view_can_go_forward(tab->view)) {
        WebKitWebResource *resource = webkit_web_view_get_main_resource(tab->view);
        WebKitURIResponse *response = resource ? webkit_web_resource_get_response(resource) : NULL;
        char *uri = g_filename_to_uri(path, NULL, NULL);
        // Only WebKit's read-only plain-text document is reloadable. HTML may
        // contain unsaved forms or scripts, regardless of its file extension.
        if (response && !g_strcmp0(webkit_uri_response_get_mime_type(response), "text/plain") &&
            uri && !g_strcmp0(uri, webkit_web_view_get_uri(tab->view)) &&
            g_file_test(path, G_FILE_TEST_IS_REGULAR))
            g_hash_table_remove(browser->tabs, key);
        g_free(uri);
    }
    g_free(key);
}

static void file_pick_free(FilePick *pick) {
    g_signal_handlers_disconnect_by_data(pick->dialog, pick);
    gtk_native_dialog_destroy(GTK_NATIVE_DIALOG(pick->dialog));
    g_object_unref(pick->dialog);
    g_free(pick->owner);
    g_free(pick);
}

static void chosen(GtkNativeDialog *dialog, int response, FilePick *pick) {
    GFile *file = response == GTK_RESPONSE_ACCEPT ? gtk_file_chooser_get_file(GTK_FILE_CHOOSER(dialog)) : NULL;
    char *path = file ? g_file_get_path(file) : NULL;
    pick->browser->pick = NULL;
    if (path) {
        JsonObject *value = json_object_new();
        json_object_set_string_member(value, "owner", pick->owner);
        json_object_set_string_member(value, "file", path);
        emit(pick->browser, value);
    }
    g_free(path);
    g_clear_object(&file);
    file_pick_free(pick);
}

static void pick_file(Browser *browser, JsonObject *message) {
    if (browser->pick || browser->download) return;
    FilePick *pick = g_new0(FilePick, 1);
    pick->browser = browser;
    pick->owner = g_strdup(string(message, "owner"));
    browser->pick = pick;
    pick->dialog = gtk_file_chooser_native_new("Open file", GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(browser->shell))),
        GTK_FILE_CHOOSER_ACTION_OPEN, "Open", "Cancel");
    const char *directory = string(message, "directory");
    if (g_path_is_absolute(directory)) {
        GFile *folder = g_file_new_for_path(directory);
        gtk_file_chooser_set_current_folder_file(GTK_FILE_CHOOSER(pick->dialog), folder, NULL);
        g_object_unref(folder);
    }
    g_signal_connect(pick->dialog, "response", G_CALLBACK(chosen), pick);
    gtk_native_dialog_show(GTK_NATIVE_DIALOG(pick->dialog));
}

static void message(WebKitUserContentManager *manager, WebKitJavascriptResult *result, Browser *browser) {
    (void)manager;
    if (g_strcmp0(webkit_web_view_get_uri(browser->shell), browser->origin)) return;
    char *json = jsc_value_to_json(webkit_javascript_result_get_js_value(result), 0);
    if (!json) return;
    JsonParser *parser = json_parser_new();
    if (strlen(json) <= 1024 * 1024 && json_parser_load_from_data(parser, json, -1, NULL)) {
        JsonNode *root = json_parser_get_root(parser);
        if (JSON_NODE_HOLDS_OBJECT(root)) {
            JsonObject *value = json_node_get_object(root);
            if (boolean(value, "pickFile")) pick_file(browser, value);
            else if (object(value, "unloadIdle")) unload_idle(browser, object(value, "unloadIdle"));
            else synchronize(browser, value);
        }
    }
    g_object_unref(parser);
    g_free(json);
}

Browser *sapiens_browser_new(WebKitWebView *shell, GtkOverlay *overlay, const char *origin, const char *data) {
    Browser *browser = g_new0(Browser, 1);
    browser->shell = g_object_ref(shell);
    browser->origin = g_strdup(origin); browser->data = g_strdup(data);
    browser->tabs = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, tab_free);
    browser->sessions = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, g_object_unref);
    browser->overlay = g_object_ref(overlay);
    WebKitUserContentManager *manager = webkit_web_view_get_user_content_manager(shell);
    g_signal_connect(manager, "script-message-received::browser", G_CALLBACK(message), browser);
    webkit_user_content_manager_register_script_message_handler(manager, "browser");
    return browser;
}

void sapiens_browser_free(Browser *browser) {
    if (browser->pick) file_pick_free(browser->pick);
    WebKitUserContentManager *manager = webkit_web_view_get_user_content_manager(browser->shell);
    g_signal_handlers_disconnect_by_data(manager, browser);
    webkit_user_content_manager_unregister_script_message_handler(manager, "browser");
    GHashTableIter iterator;
    gpointer value;
    g_hash_table_iter_init(&iterator, browser->sessions);
    while (g_hash_table_iter_next(&iterator, NULL, &value)) g_signal_handlers_disconnect_by_data(value, browser);
    g_hash_table_unref(browser->tabs);
    g_hash_table_unref(browser->sessions);
    g_object_unref(browser->overlay);
    g_object_unref(browser->shell);
    g_free(browser->origin); g_free(browser->data); g_free(browser);
}
