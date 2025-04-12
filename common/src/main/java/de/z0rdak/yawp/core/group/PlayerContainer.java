package de.z0rdak.yawp.core.group;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.constants.serialization.RegionNbtKeys;
import net.minecraft.core.UUIDUtil;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.StringTag;

import java.util.*;
import java.util.stream.Collectors;

public class PlayerContainer implements IMemberContainer {

    public static final Codec<PlayerContainer> CODEC = RecordCodecBuilder.create(
            instance -> instance.group(
                    Codec.STRING.fieldOf("name")
                            .forGetter(pc -> pc.groupName),
                    Codec.list(Codec.STRING).fieldOf("teams")
                            .forGetter(pc -> new ArrayList<>(pc.teams)),
                    Codec.unboundedMap(UUIDUtil.STRING_CODEC, Codec.STRING).fieldOf("players")
                            .forGetter(pc -> pc.players)
                    ).apply(instance, PlayerContainer::new));
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

    public PlayerContainer(String groupName, List<String> teams, Map<UUID, String> players) {
        this(groupName);
        this.teams.addAll(teams);
        this.players.putAll(players);
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
            int[] uuidInts = UUIDUtil.uuidToIntArray(uuid);
            playerNBT.putIntArray(RegionNbtKeys.UUID, uuidInts);
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
        ListTag playerLists = nbt.getList(RegionNbtKeys.PLAYERS).orElse(new ListTag());
        for (int i = 0; i < playerLists.size(); i++) {
            CompoundTag playerMapping = playerLists.getCompound(i).orElseThrow();
            var uuidInts = playerMapping.getIntArray(RegionNbtKeys.UUID).orElseThrow();
            var uuid = UUIDUtil.uuidFromIntArray(uuidInts);
            players.put(uuid, playerMapping.getString(RegionNbtKeys.NAME).orElseThrow());
        }
        // deserialize teams data
        this.teams.clear();
        ListTag teamList = nbt.getList(RegionNbtKeys.TEAMS).orElse(new ListTag());
        for (int i = 0; i < teamList.size(); i++) {
            teams.add(teamList.getString(i).orElseThrow());
        }
    }
}
