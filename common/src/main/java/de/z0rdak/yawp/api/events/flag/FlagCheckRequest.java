package de.z0rdak.yawp.api.events.flag;

import de.z0rdak.yawp.api.events.Cancelable;
import de.z0rdak.yawp.core.flag.RegionFlag;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.Nullable;

import java.util.UUID;

/**
 * Event that is fired before a flag is checked.
 * Can be used to cancel the flag check.
 */
public final class FlagCheckRequest implements Cancelable {

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
    private final RegionFlag regionFlag;

    /**
     * Internal Identifier to relate a flag check to its result.
     */
    private final String id;
    private boolean canceled;

    /**
     *
     * @param target target block position to check applicable regions for
     * @param regionFlag involved flag in the check
     * @param dimension dimension where to look for regions at the target pos
     * @param player the player to consider permissions for during check
     */
    public FlagCheckRequest(BlockPos target, RegionFlag regionFlag, ResourceKey<Level> dimension, @Nullable Player player) {
        this.player = player;
        this.target = target;
        this.dimension = dimension;
        this.regionFlag = regionFlag;
        this.id = UUID.randomUUID().toString();
    }

    /**
     *
     * @param target target block position to check applicable regions for
     * @param regionFlag involved flag in the check
     * @param dimension dimension where to look for regions at the target pos
     * @param player to consider permissions for during check
     */
    public FlagCheckRequest(BlockPos target, RegionFlag regionFlag, ResourceKey<Level> dimension, @Nullable Player player, String id) {
        this.player = player;
        this.target = target;
        this.dimension = dimension;
        this.regionFlag = regionFlag;
        this.id = id;
    }

    public FlagCheckRequest(BlockPos target, RegionFlag regionFlag, ResourceKey<Level> dimension) {
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

    public ResourceKey<Level> getDimension() {
        return dimension;
    }

    @Nullable
    public Player getPlayer() {
        return player;
    }

    @Override
    public boolean isCanceled() {
        return this.canceled;
    }

    @Override
    public void setCanceled(boolean canceled) {
        this.canceled = canceled;
    }
}