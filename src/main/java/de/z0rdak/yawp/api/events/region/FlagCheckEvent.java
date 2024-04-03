package de.z0rdak.yawp.api.events.region;

import de.z0rdak.yawp.core.flag.RegionFlag;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.eventbus.api.Cancelable;
import net.minecraftforge.eventbus.api.Event;

import javax.annotation.Nullable;

/**
 * Event that is fired before a flag is checked.
 * Can be used to cancel the flag check.
 */
@Cancelable
public class FlagCheckEvent extends Event {

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

    public FlagCheckEvent(BlockPos target, RegionFlag regionFlag, RegistryKey<World> dimension, @Nullable PlayerEntity player) {
        this.player = player;
        this.target = target;
        this.dimension = dimension;
        this.regionFlag = regionFlag;
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
