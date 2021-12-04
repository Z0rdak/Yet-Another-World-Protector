package de.z0rdak.regionshield.core.affiliation;

import java.util.UUID;

/**
 * Abstraction for a set of members
 */
public interface Division {

    boolean contains(UUID playerUUID);

    int count();

    void clear();

}
