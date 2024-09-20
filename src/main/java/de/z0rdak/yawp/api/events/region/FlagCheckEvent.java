package de.z0rdak.yawp.api.events.region;

import de.z0rdak.yawp.core.flag.RegionFlag;
import net.fabricmc.fabric.api.event.Event;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.registry.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.jetbrains.annotations.Nullable;

import java.util.UUID;

/**
 * Event that is fired before a flag is checked.
 * Can be used to cancel the flag check.
 *
 */
public final class FlagCheckEvent {

    /**
     * The target position of the flag check. Depending on the flag this can be a block position or an entity position.
     */
    private final BlockPos target;
    /**
     * The dimension in which the flag check is performed, can be used to get the corresponding Dimensional Region.
     */
    private final RegistryKey<World> dimension;
    /**
     * The player that triggered the flag check, may be null when no player was involved. This depends on the checked flag.
     */
    @Nullable
    private final PlayerEntity player;

    /**
     * The flag that is checked.
     */
    private final RegionFlag regionFlag;

    /**
     * Internal Identifier to relate a flag check to its result.
     */
    private final String id;

    public FlagCheckEvent(BlockPos target, RegionFlag regionFlag, RegistryKey<World> dimension, @Nullable PlayerEntity player) {
        this.player = player;
        this.target = target;
        this.dimension = dimension;
        this.regionFlag = regionFlag;
        this.id = UUID.randomUUID().toString();
    }

    public FlagCheckEvent(BlockPos target, RegionFlag regionFlag,  RegistryKey<World> dimension) {
        this(target, regionFlag, dimension, null);
    }
    
    public String getId() {
        return id;
    }

    public BlockPos getTarget() {
        return this.target;
    }

    public RegionFlag getRegionFlag() {
        return regionFlag;
    }

    public RegistryKey<World> getDimension() {
        return dimension;
    }

    @Nullable
    public PlayerEntity getPlayer() {
        return player;
    }
}