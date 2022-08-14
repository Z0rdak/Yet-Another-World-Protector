package de.z0rdak.regionshield.core.affiliation;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.common.util.INBTSerializable;

import java.util.*;

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
        return null;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {

    }
}
