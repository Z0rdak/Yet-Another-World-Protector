package de.z0rdak.yawp.platform.event;

import de.z0rdak.yawp.api.Flag;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;

import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.neoforged.bus.api.Event;
import net.neoforged.bus.api.ICancellableEvent;

import javax.annotation.Nullable;
import java.util.UUID;

/**
 * Event that is fired before a flag is checked.
 * Can be used to cancel the flag check.
 */
public class NeoForgeFlagCheckRequest extends Event implements ICancellableEvent {

    /**
     * The target position of the flag check. Depending on the flag this can be a block position or an entity position.
     */
    private final BlockPos target;
    /**
     * The dimension in which the flag check is performed, can be used to get the corresponding Dimensional Region.
     */
    private final ResourceKey<Level> dimension;
    /**
     * The player that triggered the flag check, may be null when no player was involved. This depends on the checked flag.
     */
    @Nullable
    private final Player player;

    /**
     * The flag that is checked.
     */
    private final Flag regionFlag;

    /**
     * Internal Identifier to relate a flag check to its result.
     */
    private final String id;


    public NeoForgeFlagCheckRequest(BlockPos target, Flag regionFlag, ResourceKey<Level> dimension, @Nullable Player player) {
        this.player = player;
        this.target = target;
        this.dimension = dimension;
        this.regionFlag = regionFlag;
        this.id = UUID.randomUUID().toString();
    }

    public NeoForgeFlagCheckRequest(FlagCheckRequest event) {
        this.player = event.getPlayer();
        this.target = event.getTarget();
        this.dimension = event.getDimension();
        this.regionFlag = event.getRegionFlag();
        this.id = event.getId();
    }

    public NeoForgeFlagCheckRequest(BlockPos target, Flag regionFlag, ResourceKey<Level> dimension, @Nullable Player player, String id) {
        this.player = player;
        this.target = target;
        this.dimension = dimension;
        this.regionFlag = regionFlag;
        this.id = id;
    }

    public NeoForgeFlagCheckRequest(BlockPos target, Flag regionFlag, ResourceKey<Level> dimension) {
        this(target, regionFlag, dimension, null);
    }

    public static FlagCheckRequest asNonEvent(NeoForgeFlagCheckRequest check) {
        return new FlagCheckRequest(check.getTarget(), check.getRegionFlag(), check.getDimension(), check.getPlayer(), check.getId());
    }

    public String getId() {
        return id;
    }

    public BlockPos getTarget() {
        return this.target;
    }

    public Flag getRegionFlag() {
        return regionFlag;
    }

    public ResourceKey<Level> getDimension() {
        return dimension;
    }

    @Nullable
    public Player getPlayer() {
        return player;
    }
}
