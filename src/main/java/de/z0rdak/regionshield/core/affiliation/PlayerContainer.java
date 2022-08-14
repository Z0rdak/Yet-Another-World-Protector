package de.z0rdak.regionshield.core.affiliation;

import de.z0rdak.regionshield.util.constants.RegionNBT;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.ListNBT;
import net.minecraft.nbt.StringNBT;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.common.util.INBTSerializable;

import java.util.*;
import java.util.stream.Collectors;

public class PlayerContainer implements IMemberContainer, INBTSerializable<CompoundNBT> {

    public Set<String> teams;
    public Map<UUID, String> players;

    public PlayerContainer(CompoundNBT nbt){
        this();
        this.deserializeNBT(nbt);
    }

    public PlayerContainer(){
        this.teams = new HashSet<>(0);
        this.players = new HashMap<>(0);
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
    public void addPlayer(PlayerEntity player) {
        this.players.put(player.getUUID(), player.getScoreboardName());
    }

    @Override
    public void addTeam(String team) {
        this.teams.add(team);
    }

    @Override
    public void removePlayer(PlayerEntity player) {
        this.players.remove(player.getUUID());
    }

    @Override
    public void removeTeam(String team) {
        this.teams.remove(team);
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        // serialize player data
        ListNBT playerList = new ListNBT();
        players.forEach( (uuid, name) -> {
            CompoundNBT playerNBT = new CompoundNBT();
            playerNBT.putUUID(RegionNBT.UUID, uuid);
            playerNBT.putString(RegionNBT.NAME, name);
            playerList.add(playerNBT);
        });
        nbt.put(RegionNBT.PLAYERS, playerList);
        // serialize team data
        ListNBT teamList = new ListNBT();
        teamList.addAll(teams.stream()
                .map(StringNBT::valueOf)
                .collect(Collectors.toSet()));
        nbt.put(RegionNBT.TEAMS, teamList);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        // deserialize players data
        this.players.clear();
        ListNBT playerLists = nbt.getList(RegionNBT.PLAYERS, Constants.NBT.TAG_COMPOUND);
        for (int i = 0; i < playerLists.size(); i++) {
            CompoundNBT playerMapping = playerLists.getCompound(i);
            players.put(playerMapping.getUUID(RegionNBT.UUID), playerMapping.getString(RegionNBT.NAME));
        }
        // deserialize teams data
        this.teams.clear();
        ListNBT teamList = nbt.getList(RegionNBT.TEAMS, Constants.NBT.TAG_STRING);
        for (int i = 0; i < teamList.size(); i++) {
            teams.add(teamList.getString(i));
        }
    }
}
