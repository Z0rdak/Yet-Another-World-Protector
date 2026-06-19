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
                    Codec.unboundedMap(UUIDUtil.STRING_CODEC, Codec.STRING).optionalFieldOf("players", new HashMap<>())
                            .forGetter(pc -> pc.players)
                    ).apply(instance, PlayerContainer::new));
    private final Map<UUID, String> players;
    private final String groupName;

    public PlayerContainer(String groupName) {
        this.groupName = groupName;
        this.players = new HashMap<>(0);
    }

    public PlayerContainer(String groupName, Map<UUID, String> players) {
        this(groupName);
        this.players.putAll(players);
    }

    @Override
    public String getGroupName() {
        return this.groupName;
    }

    public Map<UUID, String> getPlayers() {
        return players;
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
    public void addPlayer(UUID uuid, String name) {
        this.players.put(uuid, name);
    }

    @Override
    public void clearPlayers() {
        this.players.clear();
    }

    @Override
    public void removePlayer(UUID playerUUID) {
        this.players.remove(playerUUID);
    }

}
