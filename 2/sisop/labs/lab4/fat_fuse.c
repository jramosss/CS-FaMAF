/*
 * fat_fuse.c
 *
 * main() for a program to mount a FAT filesystem using FUSE
 */

#include <alloca.h>
#include <getopt.h>
#include <stdio.h>
#include <string.h>

#include "fat_fuse_ops.h"
#include "fat_volume.h"

static void usage() {
    const char *usage_str =
        "Usage: fat-fuse [-f] [-d] [-r] VOLUME MOUNTPOINT\n";
    fputs(usage_str, stdout);
}

static void usage_short() {
    const char *usage_str =
        "Usage: fat-fuse [-f] [-d] [-r] VOLUME MOUNTPOINT\n";
    fputs(usage_str, stderr);
}

static const char *shortopts = "dfhr";
static const struct option longopts[] = {
    {"debug", no_argument, NULL, 'd'},
    {"foreground", no_argument, NULL, 'f'},
    {"help", no_argument, NULL, 'h'},
    {"readonly", no_argument, NULL, 'r'},
    {NULL, 0, NULL, 0},
};

int main(int argc, char **argv) {
    char *volume;
    char *mountpoint;
    int c;
    fat_volume vol;
    char *fuse_argv[50];
    int fuse_argc;
    int fuse_status;
    int ret;
    int mount_flags = FAT_MOUNT_FLAG_READWRITE;
    int debug = 0, foreground = 0;

    while ((c = getopt_long(argc, argv, shortopts, longopts, NULL)) != -1) {
        switch (c) {
        case 'd':
            debug = 1;
            break;
        case 'f':
            foreground = 1;
            break;
        case 'h':
            usage();
            return 0;
        case 'r':
            mount_flags = FAT_MOUNT_FLAG_READONLY;
            break;
        default:
            usage_short();
            return 2;
        }
    }

    argc -= optind;
    argv += optind;
    if (argc != 2) {
        usage_short();
        return 2;
    }

    volume = argv[0];
    mountpoint = argv[1];
    fuse_argc = 0;
    fuse_argv[fuse_argc] = "fat-fuse";
    fuse_argc++;

    fuse_argv[fuse_argc] = "-s"; // Single-threaded
    fuse_argc++;

    if (mount_flags & FAT_MOUNT_FLAG_READONLY) {
        DEBUG("Read only mode");
        fuse_argv[fuse_argc] = "-o";
        fuse_argc++;
        fuse_argv[fuse_argc] = "ro";
        fuse_argc++;
    }
    if (foreground) {
        fuse_argv[fuse_argc] = "-f"; // Run in foreground
        fuse_argc++;
    }
    if (debug) {
        fuse_argv[fuse_argc] = "-d"; // Debug mode
        fuse_argc++;
    }

    fuse_argv[fuse_argc] = mountpoint;
    fuse_argc++;
    fuse_argv[fuse_argc] = NULL;

    // Mount the FAT volume with mount flags
    vol = fat_volume_mount(volume, mount_flags);
    if (!vol) {
        fat_error("Failed to mount FAT volume \"%s\": %m", volume);
        return 1;
    }

    // Call fuse_main() to pass control to FUSE.  This will daemonize the
    // process, causing it to detach from the terminal.  fat_volume_unmount()
    // will not be called until the filesystem is unmounted and fuse_main()
    // returns in the daemon process.
    fuse_status = fuse_main(fuse_argc, fuse_argv, &fat_fuse_operations, vol);
    ret = fat_volume_unmount(vol);
    if (ret)
        fat_error("failed to unmount FAT volume \"%s\": %m", volume);
    else
        ret = fuse_status;
    return ret;
}
