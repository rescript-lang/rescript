// map_api.cpp

#include <map>

extern "C" {

// Opaque handle to the map
typedef struct VoidMap VoidMap;

// Create a new map
VoidMap* create_map();

// Add key-value pair
void map_add(VoidMap* map, void* key, void* value);

// Lookup value by key
void* map_lookup(VoidMap* map, void* key);

// Free the map
void free_map(VoidMap* map);
}

struct VoidMap {
    std::map<void*, void*> map;
};

VoidMap* create_map() {
    return new VoidMap();
}

void map_add(VoidMap* map, void* key, void* value) {
    map->map[key] = value;
}

void* map_lookup(VoidMap* map, void* key) {
  auto it = map->map.find(key);
  if (it != map->map.end()) {
    return it->second;
  }

  return nullptr;
}

void free_map(VoidMap* map) {
    delete map;
}
