#include <atspi/atspi.h>
#include <stdio.h>
#include <stdlib.h>

static void names(AtspiAccessible *node, int depth, int *remaining) {
    if (depth == 0 || *remaining == 0) return;
    --*remaining;
    char *name = atspi_accessible_get_name(node, NULL);
    if (name) puts(name);
    g_free(name);
    if (atspi_accessible_get_role(node, NULL) != ATSPI_ROLE_PASSWORD_TEXT) {
        AtspiText *text = atspi_accessible_get_text_iface(node);
        if (text) {
            int length = MIN(atspi_text_get_character_count(text, NULL), 65536);
            char *value = atspi_text_get_text(text, 0, length, NULL);
            if (value) puts(value);
            g_free(value);
            g_object_unref(text);
        }
    }
    int count = atspi_accessible_get_child_count(node, NULL);
    for (int i = 0; i < count && *remaining; i++) {
        AtspiAccessible *child = atspi_accessible_get_child_at_index(node, i, NULL);
        if (!child) continue;
        names(child, depth - 1, remaining);
        g_object_unref(child);
    }
}

static gboolean snapshot(gpointer data) {
    guint pid = GPOINTER_TO_UINT(data);
    AtspiAccessible *desktop = atspi_get_desktop(0);
    if (!desktop) return G_SOURCE_CONTINUE;
    int count = atspi_accessible_get_child_count(desktop, NULL);
    puts("SNAPSHOT");
    for (int i = 0; i < count; i++) {
        AtspiAccessible *app = atspi_accessible_get_child_at_index(desktop, i, NULL);
        if (!app) continue;
        if (atspi_accessible_get_process_id(app, NULL) == pid) {
            int remaining = 1000;
            names(app, 25, &remaining);
        }
        g_object_unref(app);
    }
    g_object_unref(desktop);
    puts("END");
    fflush(stdout);
    return G_SOURCE_CONTINUE;
}

static void changed(AtspiEvent *event, void *data) {
    (void)data;
    g_boxed_free(ATSPI_TYPE_EVENT, event);
}

int main(int argc, char **argv) {
    g_setenv("ATSPI_NO_CACHE", "1", TRUE);
    if (argc != 2 || atspi_init()) return 1;
    atspi_set_timeout(1000, 1000);
    AtspiEventListener *listener = atspi_event_listener_new(changed, NULL, NULL);
    if (!atspi_event_listener_register(listener, "object:children-changed", NULL)) return 1;
    g_timeout_add(200, snapshot, GUINT_TO_POINTER((guint)strtoul(argv[1], NULL, 10)));
    atspi_event_main();
    g_object_unref(listener);
    atspi_exit();
    return 0;
}
