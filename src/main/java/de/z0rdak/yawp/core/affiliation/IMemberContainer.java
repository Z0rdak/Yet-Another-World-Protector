package de.z0rdak.yawp.core.affiliation;

import net.minecraft.world.entity.player.Player;
import net.minecraft.world.scores.Team;

import java.util.UUID;

/**
 * Abstraction for a set of members
 */
public interface IMemberContainer {

    boolean containsPlayer(UUID playerUUID);

    boolean containsTeam(String team);
    boolean containsTeam(Team team);

    void addPlayer(Player player);

    void addTeam(String team);

    void addTeam(Team team);

    boolean hasTeams();

    boolean hasPlayers();

    void removePlayer(Player player);

    void removePlayer(UUID playerUUID);

    void removeTeam(String team);

    void removeTeam(Team team);
}
