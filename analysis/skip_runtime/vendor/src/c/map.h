#ifndef MAP_H
#define MAP_H

#ifdef __cplusplus
extern "C" {
#endif

// Opaque type representing the map
typedef struct VoidMap VoidMap;

// Create a new map
VoidMap* create_map(void);

// Add a key-value pair to the map
void map_add(VoidMap* map, void* key, void* value);

// Lookup a value by key
void* map_lookup(VoidMap* map, void* key);

// Free the map
void free_map(VoidMap* map);

#ifdef __cplusplus
}
#endif

#endif // MAP_H
