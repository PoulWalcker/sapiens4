#include <gtk/gtk.h>
#include <gdk/x11/gdkx.h>
#include <X11/Xlib.h>
#include <stdio.h>
#include <unistd.h>

static GtkWindow *window;
static GtkEditable *draft;
static GMainLoop *loop;
static GtkWidget *send;

static void button_position(void) {
    GdkSurface *surface = gtk_native_get_surface(GTK_NATIVE(window));
    graphene_rect_t bounds;
    if (!gtk_widget_compute_bounds(send, GTK_WIDGET(window), &bounds)) return;
    if (!GDK_IS_X11_SURFACE(surface)) return;
    G_GNUC_BEGIN_IGNORE_DEPRECATIONS
    Display *display = gdk_x11_display_get_xdisplay(gdk_surface_get_display(surface));
    int x, y;
    int scale = gdk_surface_get_scale_factor(surface);
    Window child;
    XTranslateCoordinates(display, gdk_x11_surface_get_xid(surface), DefaultRootWindow(display),
        (bounds.origin.x + bounds.size.width / 2) * scale, (bounds.origin.y + bounds.size.height / 2) * scale, &x, &y, &child);
    G_GNUC_END_IGNORE_DEPRECATIONS
    printf("BUTTON %d %d\n", x, y);
    fflush(stdout);
}

static gboolean observed(GtkEventControllerLegacy *controller, GdkEvent *event, gpointer data) {
    (void)controller; (void)data;
    if (gdk_event_get_event_type(event) != GDK_SCROLL) return FALSE;
    double x = 0, y = 0;
    gdk_scroll_event_get_deltas(event, &x, &y);
    printf("SCROLLED %g %g\n", x, y);
    fflush(stdout);
    return FALSE;
}

static void send_message(GtkButton *button, gpointer data) {
    (void)button; (void)data;
    const char *text = gtk_editable_get_text(draft);
    char *encoded = g_base64_encode((const guchar *)text, strlen(text));
    printf("SENT %s\n", encoded);
    fflush(stdout);
    g_free(encoded);
}

static gboolean input(GIOChannel *channel, GIOCondition condition, gpointer data) {
    (void)data;
    if (condition & G_IO_HUP) { g_main_loop_quit(loop); return G_SOURCE_REMOVE; }
    char *line = NULL;
    if (g_io_channel_read_line(channel, &line, NULL, NULL, NULL) == G_IO_STATUS_NORMAL) {
        g_strchomp(line);
        if (!g_strcmp0(line, "quit")) g_main_loop_quit(loop);
        if (!g_strcmp0(line, "present")) gtk_window_present(window);
        if (!g_strcmp0(line, "hide")) gtk_widget_set_visible(GTK_WIDGET(window), FALSE);
        if (!g_strcmp0(line, "focus")) gtk_widget_grab_focus(GTK_WIDGET(draft));
        if (!g_strcmp0(line, "focus-send")) gtk_widget_grab_focus(send);
        if (!g_strcmp0(line, "point")) button_position();
        if (!g_strcmp0(line, "scale")) { printf("SCALE %d\n", gtk_widget_get_scale_factor(GTK_WIDGET(window))); fflush(stdout); }
    }
    g_free(line);
    return G_SOURCE_CONTINUE;
}

int main(void) {
    gtk_init();
    loop = g_main_loop_new(NULL, FALSE);
    window = GTK_WINDOW(gtk_window_new());
    gtk_window_set_title(window, "Blindly4 Linux Fixture");
    gtk_window_set_default_size(window, 480, 240);
    GtkWidget *box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 12);
    GtkWidget *entry = gtk_entry_new();
    draft = GTK_EDITABLE(entry);
    gtk_accessible_update_property(GTK_ACCESSIBLE(entry), GTK_ACCESSIBLE_PROPERTY_LABEL, "Draft", -1);
    GtkWidget *secret = gtk_password_entry_new();
    gtk_editable_set_text(GTK_EDITABLE(secret), "fixture-secret");
    gtk_accessible_update_property(GTK_ACCESSIBLE(secret), GTK_ACCESSIBLE_PROPERTY_LABEL, "Secret", -1);
    send = gtk_button_new_with_label("Send");
    g_signal_connect(send, "clicked", G_CALLBACK(send_message), NULL);
    g_signal_connect_swapped(entry, "activate", G_CALLBACK(send_message), NULL);
    gtk_box_append(GTK_BOX(box), entry);
    gtk_box_append(GTK_BOX(box), secret);
    gtk_box_append(GTK_BOX(box), send);
    gtk_window_set_child(window, box);
    GtkEventController *events = gtk_event_controller_legacy_new();
    gtk_event_controller_set_propagation_phase(events, GTK_PHASE_CAPTURE);
    g_signal_connect(events, "event", G_CALLBACK(observed), NULL);
    gtk_widget_add_controller(GTK_WIDGET(window), events);
    GIOChannel *channel = g_io_channel_unix_new(STDIN_FILENO);
    guint source = g_io_add_watch(channel, G_IO_IN | G_IO_HUP, input, NULL);
    gtk_window_present(window);
    gtk_widget_grab_focus(entry);
    printf("READY %d\n", getpid());
    fflush(stdout);
    g_main_loop_run(loop);
    g_source_remove(source);
    g_io_channel_unref(channel);
    gtk_window_destroy(window);
    g_main_loop_unref(loop);
    return 0;
}
