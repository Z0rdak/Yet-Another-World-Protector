package de.z0rdak.yawp.core.group;

import de.z0rdak.yawp.core.INbtSerializable;
import net.minecraft.nbt.NbtCompound;

import java.util.UUID;

/**
 * Abstraction for a set of members
 */
public interface IMemberContainer extends INbtSerializable<NbtCompound> {

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
