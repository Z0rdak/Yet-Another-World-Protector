package de.z0rdak.yawp.core.group;

import net.minecraft.nbt.CompoundTag;
import net.neoforged.neoforge.common.util.INBTSerializable;

import java.util.UUID;

/**
 * Abstraction for a set of members
 */
public interface IMemberContainer extends INBTSerializable<CompoundTag> {

    boolean hasPlayer(UUID playerUUID);

    boolean hasTeam(String team);

    void addPlayer(UUID uuid, String name);

    void addTeam(String team);

    boolean hasTeams();

    boolean hasPlayers();

    void clearPlayers();

    void removePlayer(UUID playerUUID);

    void removeTeam(String team);

    void clearTeams();
}
