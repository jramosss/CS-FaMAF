#include <assert.h>
#include <stdlib.h>
#include "map.h"

struct _node_t {
    map_t left;
    map_t right;
    key_t key;
    value_t value;
};

map_t map_empty() {
    return NULL;
}

static map_t create_node(key_t key, value_t value, map_t left, map_t right) {
    map_t node = calloc(1,sizeof(struct _node_t));
    node->key = key;
    node->value = value;
    node->left = left;
    node->right = right;
    return (node);
}

static map_t free_node(map_t node) {
    node->key = key_destroy(node->key);
    node->value = value_destroy(node->value);
    free(node);
    node = NULL;
    return (NULL);
}

map_t take_me_there (map_t map, key_t key) {
    assert(key != NULL);
    map_t aux = NULL;
    if (map != NULL) {
        if (map_contains(map,key)) {
            if (key_eq(map->key,key)) {
                aux = map;
            }
            else if (key_less(map->key,key)){
                aux = take_me_there(map->right,key);
            }
            else {
                aux = take_me_there(map->left,key);
            }
        }
    }
    return aux;
}

map_t map_put(map_t map, key_t key, value_t value) {
    assert(key != NULL);
    assert(value != NULL);
    map_t node = map;
    if (map == NULL){
        node = create_node(key,value,NULL,NULL);
    }
    else if (map_contains(map,key)) {
        node = take_me_there (map,key);
        key_destroy(node->key);
        node->key = key;
        value_destroy(node->value);
        node->value = value;
    }
    else {
        if (key_less(key,map->key)) { 
             node->left = map_put(node->left,key,value);
        }
        else {
            node->right = map_put(node->right,key,value);
        }
    }
    return (node);
} 

value_t map_get(map_t map, key_t key) {
    assert(key != NULL);
    value_t value = NULL;
    if (map != NULL) {
        if (map_contains(map,key)) {
            map = take_me_there(map,key);
            value = map->value;
        }
    }
    return value;
}

bool map_contains(map_t map, key_t key) {
    assert(key != NULL);
    bool b = false;
    if (map != NULL) {
        if (key_eq(map->key,key)){
            b = true;
        }
        else {
            b = map_contains(map->left,key) || map_contains(map->right,key);
        }
    }
    return b;
}

map_t map_remove (map_t map, key_t key) {
    map_t result = map;
    if(key_eq(map->key,key)){
        if(map->left == NULL) {
            result = map->right;
            map = free_node(map);
        }
        else {
            map_t father = NULL;
            map_t max = map->left;
            while (max->right != NULL) {
                father = max;
                max = max->right;
            } 
            if (father == NULL){
                max->right = map->right;
                map = free_node(map);
                result = max;
            }
            else {
                map->key = key_destroy(map->key);
                map->value = value_destroy(map->value);
                map->key = max->key;
                map->value = max->value;
                father->right=max->left;
                free(max);
            }
        }
    }
    return result;
}

map_t map_destroy(map_t map) {
    if (map != NULL) {
        map_destroy(map->left);
        map_destroy(map->right);
        free(map);
        map = NULL;
    }
    return (map);
}

void map_dump(map_t map, FILE *file) {
    if (map != NULL) {
        map_dump(map->left, file);
        key_dump(map->key, file);
        fprintf(file, ": ");
        value_dump(map->value, file);
        fprintf(file, "\n");
        map_dump(map->right, file);
    }
}
