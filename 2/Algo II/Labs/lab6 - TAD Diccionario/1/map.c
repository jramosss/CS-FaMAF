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
    map_t node = calloc (1, sizeof (struct _node_t));
    node -> key = key;
    node -> value = value;
    node -> right = right;
    node -> left = left;
    return (node);
}

static map_t free_node(map_t node) {
    key_destroy(node -> key);
    value_destroy(node -> value);
    free (node);
    return (NULL);
}

map_t take_me_there (map_t map, key_t key) {
    map_t node = NULL;
    if (map != NULL) {
        if (key_eq(map -> key, key)) {
            node = map;
        }
        else if (key_less(key,map->key)){
            node = take_me_there(map->left,key);
        }
        else {
            node = take_me_there(map->right,key);
        }
    }
    return node;
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
    map_dump(node, stdout);   
    return (node);
} 
value_t map_get(map_t map, key_t key) {
    assert(key != NULL);
    value_t value = NULL;
    map_t node = map;
    if (map != NULL){
        if (key_eq(node->key, key)) {
            value = node->value;
        }
        else if (key_less(key,(node->key))){
            value = map_get(node->left,key);
        }
        else {
            value = map_get(node->right,key);
        }
    }
    return (value);
}

bool map_contains(map_t map, key_t key) {
    assert(key != NULL);
    bool x = false;
    if (map == NULL) {
        x = false;
    }
    else {
        if (key_eq(map->key, key)) {
            x = true;
        }
        else {
            x = map_contains (map->right, key) || map_contains (map->left, key);
        }
    }
    return x;
}


map_t map_remove(map_t map, key_t key) {
    map_t node = NULL;
    map_t aux = NULL;
    if(map_contains(map,key)){
            node = take_me_there(map,key);
            aux = node;
            while (aux->right != NULL) {
                aux = aux->right;
            }
            if (aux->left == NULL) {
                node->value = aux->value;
                node->key = aux->key;
                aux = free_node(aux);
                aux = NULL;
            }
            else {
                aux=map_remove(aux,aux->key);
            }
        }
    return map;
}

map_t map_remove2 (map_t map, key_t key) {
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
    if (map != NULL){
        map_destroy(map->left);
        map_destroy(map->right);
        free_node(map);
    }
    return (NULL);
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
