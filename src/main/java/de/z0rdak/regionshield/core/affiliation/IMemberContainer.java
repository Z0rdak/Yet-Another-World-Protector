package de.z0rdak.regionshield.core.affiliation;

import net.minecraft.entity.player.PlayerEntity;

import java.util.UUID;

/**
 * Abstraction for a set of members
 */
public interface IMemberContainer {

    boolean containsPlayer(UUID playerUUID);

    boolean containsTeam(String team);

    void addPlayer(PlayerEntity player);

    void addTeam(String team);

    void removePlayer(PlayerEntity player);

    void removeTeam(String team);
}
