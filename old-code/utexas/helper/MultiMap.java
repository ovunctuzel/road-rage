/*
 * AORTA: Approximately Orchestrated Road Traffic Automata
 * Copyright (C) 2011 UT-Austin & Austin Robot Technology, Dustin Carlino, Mike Depinet
 * License: Modified BSD Software License
 */

package utexas.helper;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

// Map one key to many values.
public class MultiMap<K, V> {
    private HashMap<K, LinkedList<V>> map = new HashMap<K, LinkedList<V>>();

    public List<V> get (K key) {
        return map.get(key);
    }

    public void put (K key, V value) {
        LinkedList<V> ls;
        if (map.containsKey(key)) {
            ls = map.get(key);
        } else {
            ls = new LinkedList<V>();
            map.put(key, ls);
        }
        ls.add(value);
    }

    public Set<K> keySet () {
        return map.keySet();
    }

    public void clear () {
        map.clear();
    }
}
