package de.z0rdak.yawp.core.affiliation;

import de.z0rdak.yawp.core.INbtSerializable;
import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtElement;
import net.minecraft.nbt.NbtList;
import net.minecraft.nbt.NbtString;
import net.minecraft.scoreboard.Team;

import java.util.*;
import java.util.stream.Collectors;

public class PlayerContainer implements IMemberContainer, INbtSerializable<NbtCompound> {

    private final Set<String> teams;
    private final Map<UUID, String> players;

    public PlayerContainer(NbtCompound nbt) {
        this();
        this.deserializeNBT(nbt);
    }

    public PlayerContainer() {
        this.teams = new HashSet<>(0);
        this.players = new HashMap<>(0);
    }

    public Set<String> getTeams() {
        return teams;
    }

    public Map<UUID, String> getPlayers() {
        return players;
    }

    @Override
    public boolean hasTeams() {
        return !this.teams.isEmpty();
    }

    @Override
    public boolean hasPlayers() {
        return !this.players.isEmpty();
    }

    @Override
    public boolean containsPlayer(UUID playerUUID) {
        return this.players.containsKey(playerUUID);
    }

    @Override
    public boolean containsTeam(String team) {
        return this.teams.contains(team);
    }

    @Override
    public boolean containsTeam(Team team) {
        return this.teams.contains(team.getName());
    }

    @Override
    public void addPlayer(PlayerEntity player) {
        this.players.put(player.getUuid(), player.getName().toString());
    }

    @Override
    public void addTeam(String team) {
        this.teams.add(team);
    }

    @Override
    public void addTeam(Team team) {
        this.teams.add(team.getName());
    }

    @Override
    public void removePlayer(PlayerEntity player) {
        this.players.remove(player.getUuid());
    }

    @Override
    public void removePlayer(UUID playerUUID) {
        this.players.remove(playerUUID);
    }

    @Override
    public void removeTeam(String team) {
        this.teams.remove(team);
    }

    @Override
    public void removeTeam(Team team) {
        this.teams.remove(team.getName());
    }

    @Override
    public NbtCompound serializeNBT() {
        NbtCompound nbt = new NbtCompound();
        // serialize player data
        NbtList playerList = new NbtList();
        players.forEach((uuid, name) -> {
            NbtCompound playerNBT = new NbtCompound();
            playerNBT.putUuid(RegionNBT.UUID, uuid);
            playerNBT.putString(RegionNBT.NAME, name);
            playerList.add(playerNBT);
        });
        nbt.put(RegionNBT.PLAYERS, playerList);
        // serialize team data
        NbtList teamList = new NbtList();
        teamList.addAll(teams.stream()
                .map(NbtString::of)
                .collect(Collectors.toSet()));
        nbt.put(RegionNBT.TEAMS, teamList);
        return nbt;
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        // deserialize players data
        this.players.clear();
        NbtList playerLists = nbt.getList(RegionNBT.PLAYERS, NbtElement.COMPOUND_TYPE);
        for (int i = 0; i < playerLists.size(); i++) {
            NbtCompound playerMapping = playerLists.getCompound(i);
            players.put(playerMapping.getUuid(RegionNBT.UUID), playerMapping.getString(RegionNBT.NAME));
        }
        // deserialize teams data
        this.teams.clear();
        NbtList teamList = nbt.getList(RegionNBT.TEAMS, NbtElement.STRING_TYPE);
        for (int i = 0; i < teamList.size(); i++) {
            teams.add(teamList.getString(i));
        }
    }
}
