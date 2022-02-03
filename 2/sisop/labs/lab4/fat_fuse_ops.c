/*
 * fat_fuse_ops.c
 *
 * FAT32 filesystem operations for FUSE (Filesystem in Userspace)
 */
#include "fat_fuse_ops.h"
#include "fat_file.h"
#include "fat_filename_util.h"
#include "fat_fs_tree.h"
#include "fat_util.h"
#include "fat_volume.h"
#include <alloca.h>
#include <errno.h>
#include <gmodule.h>
#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include "censorship.h"

#define HIDDEN_FILE_PATH "/.fs.log"
#define HIDDEN_FILE_NAME ".fs.log"
#define OP_READ "READ"
#define OP_WRITE "WRITE"


static int fat_fuse_mknod(const char *path, mode_t mode, dev_t dev);

/* Retrieve the currently mounted FAT volume from the FUSE context. */
static inline fat_volume get_fat_volume() {
    return fuse_get_context()->private_data;
}

//Use these functions to build the error logs

#define LOG_MESSAGE_SIZE 100

static void now_to_str(char *buf) {
    time_t now = time(NULL);
    struct tm *timeinfo;
    timeinfo = localtime(&now);

    strftime(buf, 30, "%d-%m-%Y %H:%M", timeinfo);
}

off_t fs_offset = 0;

static void fat_fuse_log_activity(char *operation_type, fat_file file) {
    char buf[LOG_MESSAGE_SIZE] = "";
    now_to_str(buf);
    strcat(buf, "\t");
    strcat(buf, file->filepath);
    strcat(buf, "\t");
    strcat(buf, operation_type);
    strcat(buf, "\n");

    //Obtenemos el file_tree
    fat_tree tree = get_fat_volume()->file_tree;
    //Obtenemos el archivo .fs.log
    fat_file fslog = fat_tree_search(tree,HIDDEN_FILE_PATH);
    //Checkeos que no estan de mas, tiran SEGFAULT
    if (fslog == NULL) printf("No existe fslog en el arbol\n");
    //Obtenemos el directorio padre de nuestro archivo .fs.log
    fat_file parent = fat_tree_get_parent(
                fat_tree_node_search(tree,HIDDEN_FILE_PATH));
    if (parent == NULL) printf("No existe el parent lol\n");
    //Escribimos nuestro log en .fs.log
    fat_file_pwrite(fslog,buf,LOG_MESSAGE_SIZE,fs_offset,parent);
    //Aumentamos el offset en lo que hayamos escrito
    fs_offset += (off_t)strlen(buf);
}

/* Get file attributes (file descriptor version) */
static int fat_fuse_fgetattr(const char *path, struct stat *stbuf,
                             struct fuse_file_info *fi) {
    fat_file file = (fat_file)fat_tree_get_file((fat_tree_node)fi->fh);
    fat_file_to_stbuf(file, stbuf);
    return 0;
}

/* Get file attributes (path version) */
static int fat_fuse_getattr(const char *path, struct stat *stbuf) {
    fat_volume vol;
    fat_file file;

    vol = get_fat_volume();
    file = fat_tree_search(vol->file_tree, path);
    if (file == NULL) {
        errno = ENOENT;
        return -errno;
    }
    fat_file_to_stbuf(file, stbuf);
    return 0;
}

/* Open a file */
static int fat_fuse_open(const char *path, struct fuse_file_info *fi) {
    fat_volume vol;
    fat_tree_node file_node;
    fat_file file;

    vol = get_fat_volume();
    file_node = fat_tree_node_search(vol->file_tree, path);
    if (!file_node)
        return -errno;
    file = fat_tree_get_file(file_node);
    if (fat_file_is_directory(file))
        return -EISDIR;
    fat_tree_inc_num_times_opened(file_node);
    fi->fh = (uintptr_t)file_node;
    return 0;
}

/* Open a directory */
static int fat_fuse_opendir(const char *path, struct fuse_file_info *fi) {
    fat_volume vol = NULL;
    fat_tree_node file_node = NULL;
    fat_file file = NULL;

    vol = get_fat_volume();
    file_node = fat_tree_node_search(vol->file_tree, path);
    if (file_node == NULL) {
        return -errno;
    }
    file = fat_tree_get_file(file_node);
    if (!fat_file_is_directory(file)) {
        return -ENOTDIR;
    }
    fat_tree_inc_num_times_opened(file_node);
    fi->fh = (uintptr_t)file_node;
    return 0;
}

/* Read directory children */
static void fat_fuse_read_children(fat_tree_node dir_node) {
    fat_volume vol = get_fat_volume();
    fat_file dir = fat_tree_get_file(dir_node);
    GList *children_list = fat_file_read_children(dir);
    // Add child to tree. TODO handle duplicates
    for (GList *l = children_list; l != NULL; l = l->next) {
        vol->file_tree =
            fat_tree_insert(vol->file_tree, dir_node, (fat_file)l->data);
    }
}

/* Add entries of a directory in @fi to @buf using @filler function. */
static int fat_fuse_readdir(const char *path, void *buf, fuse_fill_dir_t filler,
                            off_t offset, struct fuse_file_info *fi) {
    errno = 0;
    fat_tree_node dir_node = (fat_tree_node)fi->fh;
    fat_file dir = fat_tree_get_file(dir_node);
    fat_file *children = NULL, *child = NULL;
    bool hidden_file_exists = false;
    int error = 0;

    // Insert first two filenames (. and ..)
    if ((*filler)(buf, ".", NULL, 0) || (*filler)(buf, "..", NULL, 0)) {
        return -errno;
    }
    if (!fat_file_is_directory(dir)) {
        errno = ENOTDIR;
        return -errno;
    }
    if (dir->children_read != 1) {
        fat_fuse_read_children(dir_node);
        if (errno < 0) {
            return -errno;
        }
    }

    children = fat_tree_flatten_h_children(dir_node);
    child = children;
    while (*child != NULL) {
        error = (*filler)(buf, (*child)->name, NULL, 0);
        //Buscamos el archivo llamado .fs.log
        if (strcmp((*child)->name,HIDDEN_FILE_NAME) == 0){
            //Lo encontramos!
            //!PARTE 3.3
            (*child)->dentry->attribs = FILE_ATTRIBUTE_SYSTEM;
            //marcamos el archivo como "Listo para eliminarse"
            (*child)->dentry->base_name[0] = 0xe5;
            //Marcamos que ya existe un archivo con tal nombre
            hidden_file_exists = true;
        }
        if (error != 0) {
            return -errno;
        }
        child++;
    }
    //!Parte 3.1
    //Si no existe un archivo con el nombre .fs.log
    if (!hidden_file_exists){
        //Entonces lo creamos
        fat_fuse_mknod(HIDDEN_FILE_PATH,0,0);
    }
   
    return 0;
}

/* Read data from a file */
static int fat_fuse_read(const char *path, char *buf, size_t size, off_t offset,
                         struct fuse_file_info *fi) {
    errno = 0;
    int bytes_read;
    fat_tree_node file_node = (fat_tree_node)fi->fh;
    fat_file file = fat_tree_get_file(file_node);
    fat_file parent = fat_tree_get_parent(file_node);

    bytes_read = fat_file_pread(file, buf, size, offset, parent);

    //!Parte 3.2
    fat_fuse_log_activity(OP_READ,file);

    if (errno != 0) {
        return -errno;
    }
    //!PRIMERA PARTE
    u32 k = 0;
    u32 start = 0;
    //For que recorre los caracteres dentro del buffer
    for (u32 i = 0; i < bytes_read; i++){
        //Si el caracter representa el final de una palabra
        if ((buf[i] == '.') || (buf[i] == ' ') || (buf[i] == ',') ||
            (buf[i] == '(') || (buf[i] == ')') || (buf[i] == ';') ||
            (buf[i] == '\n') || (buf[i] == '\r') || (buf[i] == '\t') 
            || (buf[i] == ':') ){
            k = 0;
            u32 len = i - start;
            if (!len){
                start++;
                continue;
            }
            char * word = malloc(len);
            //Checkeo que nunca esta de mas
            if (word == NULL) printf("OOM\n");
            
            //Guardo la palabra
            for (u32 j = start; j < i; j++){
                word[k] = buf[j];
                k++;
            }
            //Me fijo si la palabra esta en prohibited
            for (u32 k = 0; k < 110; k++){
                if (strcmp(word,prohibited[k]) == 0){
                    for (u32 p = 0 ; p < strlen(word); p++)
                        buf[start+p] = 'x';
                }
            }        
            start = i+1;
            free(word);
        }
    }
    return bytes_read;
}

/* Write data from a file */
static int fat_fuse_write(const char *path, const char *buf, size_t size,
                          off_t offset, struct fuse_file_info *fi) {
    fat_tree_node file_node = (fat_tree_node)fi->fh;
    fat_file file = fat_tree_get_file(file_node);
    fat_file parent = fat_tree_get_parent(file_node);   

    //!Parte 3.2
    fat_fuse_log_activity(OP_WRITE,file);

    if (size == 0)
        return 0; // Nothing to write
    if (offset > file->dentry->file_size)
        return -EOVERFLOW;

    printf("\t IN WRITE %s %lu %lu\n", file->filepath, size, offset);
    return fat_file_pwrite(file, buf, size, offset, parent);
}

/* Close a file */
static int fat_fuse_release(const char *path, struct fuse_file_info *fi) {
    fat_tree_node file = (fat_tree_node)fi->fh;
    fat_tree_dec_num_times_opened(file);
    return 0;
}

/* Close a directory */
static int fat_fuse_releasedir(const char *path, struct fuse_file_info *fi) {
    fat_tree_node file = (fat_tree_node)fi->fh;
    fat_tree_dec_num_times_opened(file);
    return 0;
}

static int fat_fuse_mkdir(const char *path, mode_t mode) {
    errno = 0;
    fat_volume vol = NULL;
    fat_file parent = NULL, new_file = NULL;
    fat_tree_node parent_node = NULL;

    // The system has already checked the path does not exist. We get the parent
    vol = get_fat_volume();
    parent_node = fat_tree_node_search(vol->file_tree, dirname(strdup(path)));
    if (parent_node == NULL) {
        errno = ENOENT;
        return -errno;
    }
    parent = fat_tree_get_file(parent_node);
    if (!fat_file_is_directory(parent)) {
        fat_error("Error! Parent is not directory\n");
        errno = ENOTDIR;
        return -errno;
    }

    // init child
    new_file = fat_file_init(vol->table, true, strdup(path));
    if (errno != 0) {
        return -errno;
    }
    // insert to directory tree representation
    vol->file_tree = fat_tree_insert(vol->file_tree, parent_node, new_file);
    // write file in parent's entry (disk)
    fat_file_dentry_add_child(parent, new_file);
    return -errno;
}

/* Creates a new file in @path. @mode and @dev are ignored. */
static int fat_fuse_mknod(const char *path, mode_t mode, dev_t dev) {
    errno = 0;
    fat_volume vol;
    fat_file parent, new_file;
    fat_tree_node parent_node;

    // The system has already checked the path does not exist. We get the parent
    vol = get_fat_volume();
    parent_node = fat_tree_node_search(vol->file_tree, dirname(strdup(path)));
    if (parent_node == NULL) {
        errno = ENOENT;
        return -errno;
    }
    parent = fat_tree_get_file(parent_node);
    if (!fat_file_is_directory(parent)) {
        fat_error("Error! Parent is not directory\n");
        errno = ENOTDIR;
        return -errno;
    }
    new_file = fat_file_init(vol->table, false, strdup(path));
    if (errno < 0) {
        return -errno;
    }
    // insert to directory tree representation
    vol->file_tree = fat_tree_insert(vol->file_tree, parent_node, new_file);
    fat_file_dentry_add_child(parent, new_file);
    return -errno;
}

static int fat_fuse_utime(const char *path, struct utimbuf *buf) {
    errno = 0;
    fat_file parent = NULL;
    fat_volume vol = get_fat_volume();
    fat_tree_node file_node = fat_tree_node_search(vol->file_tree, path);
    if (file_node == NULL || errno != 0) {
        errno = ENOENT;
        return -errno;
    }
    parent = fat_tree_get_parent(file_node);
    if (parent == NULL || errno != 0) {
        DEBUG("WARNING: Setting time for parent ignored");
        return 0; // We do nothing, no utime for parent
    }
    fat_utime(fat_tree_get_file(file_node), parent, buf);
    return -errno;
}

/* Shortens the file at the given offset.*/
int fat_fuse_truncate(const char *path, off_t offset) {
    errno = 0;
    fat_volume vol = get_fat_volume();
    fat_file file = NULL, parent = NULL;
    fat_tree_node file_node = fat_tree_node_search(vol->file_tree, path);
    if (file_node == NULL || errno != 0) {
        errno = ENOENT;
        return -errno;
    }
    file = fat_tree_get_file(file_node);
    if (fat_file_is_directory(file))
        return -EISDIR;

    parent = fat_tree_get_parent(file_node);
    fat_tree_inc_num_times_opened(file_node);
    fat_file_truncate(file, offset, parent);
    return -errno;
}

/* Filesystem operations for FUSE.  Only some of the possible operations are
 * implemented (the rest stay as NULL pointers and are interpreted as not
 * implemented by FUSE). */
struct fuse_operations fat_fuse_operations = {
    .fgetattr = fat_fuse_fgetattr,
    .getattr = fat_fuse_getattr,
    .open = fat_fuse_open,
    .opendir = fat_fuse_opendir,
    .mkdir = fat_fuse_mkdir,
    .mknod = fat_fuse_mknod,
    .read = fat_fuse_read,
    .readdir = fat_fuse_readdir,
    .release = fat_fuse_release,
    .releasedir = fat_fuse_releasedir,
    .utime = fat_fuse_utime,
    .truncate = fat_fuse_truncate,
    .write = fat_fuse_write,

/* We use `struct fat_file_s's as file handles, so we do not need to
 * require that the file path be passed to operations such as read() */
#if FUSE_MAJOR_VERSION > 2 || \
    (FUSE_MAJOR_VERSION == 2 && FUSE_MINOR_VERSION >= 8)
    .flag_nullpath_ok = 1,
#endif
#if FUSE_MAJOR_VERSION > 2 || \
    (FUSE_MAJOR_VERSION == 2 && FUSE_MINOR_VERSION >= 9)
    .flag_nopath = 1,
#endif
};
