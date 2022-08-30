package de.z0rdak.yawp.core.affiliation;

import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.StringTag;
import net.minecraft.nbt.Tag;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.scores.Team;
import net.minecraftforge.common.util.INBTSerializable;

import java.util.*;
import java.util.stream.Collectors;

public class PlayerContainer implements IMemberContainer, INBTSerializable<CompoundTag> {

    private Set<String> teams;
    private Map<UUID, String> players;

    public PlayerContainer(CompoundTag nbt){
        this();
        this.deserializeNBT(nbt);
    }

    public PlayerContainer(){
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
    public void addPlayer(Player player) {
        this.players.put(player.getUUID(), player.getScoreboardName());
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
    public void removePlayer(Player player) {
        this.players.remove(player.getUUID());
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
    public CompoundTag serializeNBT() {
        CompoundTag nbt = new CompoundTag();
        // serialize player data
        ListTag playerList = new ListTag();
        players.forEach( (uuid, name) -> {
            CompoundTag playerNBT = new CompoundTag();
            playerNBT.putUUID(RegionNBT.UUID, uuid);
            playerNBT.putString(RegionNBT.NAME, name);
            playerList.add(playerNBT);
        });
        nbt.put(RegionNBT.PLAYERS, playerList);
        // serialize team data
        ListTag teamList = new ListTag();
        teamList.addAll(teams.stream()
                .map(StringTag::valueOf)
                .collect(Collectors.toSet()));
        nbt.put(RegionNBT.TEAMS, teamList);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        // deserialize players data
        this.players.clear();
        ListTag playerLists = nbt.getList(RegionNBT.PLAYERS, Tag.TAG_COMPOUND);
        for (int i = 0; i < playerLists.size(); i++) {
            CompoundTag playerMapping = playerLists.getCompound(i);
            players.put(playerMapping.getUUID(RegionNBT.UUID), playerMapping.getString(RegionNBT.NAME));
        }
        // deserialize teams data
        this.teams.clear();
        ListTag teamList = nbt.getList(RegionNBT.TEAMS, Tag.TAG_STRING);
        for (int i = 0; i < teamList.size(); i++) {
            teams.add(teamList.getString(i));
        }
    }
}
