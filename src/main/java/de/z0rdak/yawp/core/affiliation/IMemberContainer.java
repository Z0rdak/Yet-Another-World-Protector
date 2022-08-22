package de.z0rdak.yawp.core.affiliation;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.scoreboard.Team;

import java.util.UUID;

/**
 * Abstraction for a set of members
 */
public interface IMemberContainer {

    boolean containsPlayer(UUID playerUUID);

    boolean containsTeam(String team);
    boolean containsTeam(Team team);

    void addPlayer(PlayerEntity player);

    void addTeam(String team);

    void addTeam(Team team);

    boolean hasTeams();

    boolean hasPlayers();

    void removePlayer(PlayerEntity player);

    void removePlayer(UUID playerUUID);

    void removeTeam(String team);

    void removeTeam(Team team);
}
