package de.z0rdak.yawp.core.group;

import de.z0rdak.yawp.constants.serialization.RegionNbtKeys;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.StringTag;
import net.minecraft.nbt.Tag;

import java.util.*;
import java.util.stream.Collectors;

public class PlayerContainer implements IMemberContainer {

    private final Set<String> teams;
    private final Map<UUID, String> players;
    private final String groupName;

    public PlayerContainer(CompoundTag nbt) {
        this("n/a");
        this.deserializeNBT(nbt);
    }

    public PlayerContainer(String groupName) {
        this.groupName = groupName;
        this.teams = new HashSet<>(0);
        this.players = new HashMap<>(0);
    }

    @Override
    public String getGroupName() {
        return this.groupName;
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
    public boolean hasPlayer(UUID playerUUID) {
        return this.players.containsKey(playerUUID);
    }

    @Override
    public boolean hasTeam(String team) {
        return this.teams.contains(team);
    }

    @Override
    public void addPlayer(UUID uuid, String name) {
        this.players.put(uuid, name);
    }

    @Override
    public void addTeam(String team) {
        this.teams.add(team);
    }

    @Override
    public void clearPlayers() {
        this.players.clear();
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
    public void clearTeams() {
        this.teams.clear();
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = new CompoundTag();
        // serialize player data
        ListTag playerList = new ListTag();
        players.forEach((uuid, name) -> {
            CompoundTag playerNBT = new CompoundTag();
            playerNBT.putUUID(RegionNbtKeys.UUID, uuid);
            playerNBT.putString(RegionNbtKeys.NAME, name);
            playerList.add(playerNBT);
        });
        nbt.put(RegionNbtKeys.PLAYERS, playerList);
        // serialize team data
        ListTag teamList = new ListTag();
        teamList.addAll(teams.stream()
                .map(StringTag::valueOf)
                .collect(Collectors.toSet()));
        nbt.put(RegionNbtKeys.TEAMS, teamList);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        // deserialize players data
        this.players.clear();
        ListTag playerLists = nbt.getList(RegionNbtKeys.PLAYERS, Tag.TAG_COMPOUND);
        for (int i = 0; i < playerLists.size(); i++) {
            CompoundTag playerMapping = playerLists.getCompound(i);
            players.put(playerMapping.getUUID(RegionNbtKeys.UUID), playerMapping.getString(RegionNbtKeys.NAME));
        }
        // deserialize teams data
        this.teams.clear();
        ListTag teamList = nbt.getList(RegionNbtKeys.TEAMS, Tag.TAG_STRING);
        for (int i = 0; i < teamList.size(); i++) {
            teams.add(teamList.getString(i));
        }
    }
}
