#include <gtk/gtk.h>
#include <json-glib/json-glib.h>
#include <unistd.h>

int sapiens_linux_available(void);
int sapiens_linux_run(const char *url, const char *data);

static gboolean input(GIOChannel *channel, GIOCondition condition, gpointer data) {
    (void)data;
    GList *windows = gtk_window_list_toplevels();
    GtkWindow *window = windows ? GTK_WINDOW(windows->data) : NULL;
    g_list_free(windows);
    if (!window) return G_SOURCE_CONTINUE;
    if (condition & G_IO_HUP) {
        gtk_window_close(window);
        return G_SOURCE_CONTINUE;
    }
    char *line = NULL;
    if (g_io_channel_read_line(channel, &line, NULL, NULL, NULL) == G_IO_STATUS_NORMAL) {
        JsonParser *parser = json_parser_new();
        if (json_parser_load_from_data(parser, line, -1, NULL) && JSON_NODE_HOLDS_OBJECT(json_parser_get_root(parser))) {
            JsonObject *object = json_node_get_object(json_parser_get_root(parser));
            if (json_object_has_member(object, "width") && json_object_has_member(object, "height"))
                gtk_window_resize(window, json_object_get_int_member(object, "width"), json_object_get_int_member(object, "height"));
        }
        g_object_unref(parser);
    }
    g_free(line);
    return G_SOURCE_CONTINUE;
}

int main(int argc, char **argv) {
    if (argc != 3 || !sapiens_linux_available()) return 1;
    GIOChannel *channel = g_io_channel_unix_new(STDIN_FILENO);
    guint source = g_io_add_watch(channel, G_IO_IN | G_IO_HUP, input, NULL);
    int status = sapiens_linux_run(argv[1], argv[2]);
    g_source_remove(source);
    g_io_channel_unref(channel);
    return status;
}
