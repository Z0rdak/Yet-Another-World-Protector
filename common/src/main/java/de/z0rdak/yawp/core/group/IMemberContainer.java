package de.z0rdak.yawp.core.group;

import java.util.UUID;

/**
 * Abstraction for a set of members
 */
public interface IMemberContainer {

    String getGroupName();

    boolean hasPlayer(UUID playerUUID);

    void addPlayer(UUID uuid, String name);

    boolean hasPlayers();

    void clearPlayers();

    void removePlayer(UUID playerUUID);
}
