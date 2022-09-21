package de.z0rdak.yawp.api.events.region;

import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraftforge.eventbus.api.Cancelable;
import net.minecraftforge.eventbus.api.Event;

import javax.annotation.Nullable;

public abstract class RegionEvent extends Event {

    private final IMarkableRegion region;
    @Nullable
    private final PlayerEntity player;

    private RegionEvent(IMarkableRegion region, PlayerEntity player) {
        this.region = region;
        this.player = player;
    }

    public IMarkableRegion getRegion() {
        return region;
    }

    public PlayerEntity getPlayer() {
        return player;
    }


    /**
     * This event is fired whenever a new region is created. This event is cancelable.
     * When this is event is canceled, the region will not be created.
     */
    @Cancelable
    public static class CreateRegionEvent extends RegionEvent {

        public CreateRegionEvent(IMarkableRegion region, PlayerEntity player) {
            super(region, player);
        }

    }

    /**
     * This event is fired whenever a region is about to be removed. This event is cancelable.
     * When this is event is canceled, the region will not be deleted.
     */
    @Cancelable
    public static class RemoveRegionEvent extends RegionEvent {

        public RemoveRegionEvent(IMarkableRegion region, PlayerEntity player) {
            super(region, player);
        }
    }

    /**
     * This event is fired whenever a region is updated. This event is cancelable.
     * When this is event is canceled, the region properties will not be changed.
     */
    @Cancelable
    public static class UpdateRegionEvent extends RegionEvent {

        public UpdateRegionEvent(IMarkableRegion region, PlayerEntity player) {
            super(region, player);
        }
    }
}


