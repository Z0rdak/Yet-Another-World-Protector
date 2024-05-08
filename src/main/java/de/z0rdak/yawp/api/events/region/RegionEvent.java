package de.z0rdak.yawp.api.events.region;

import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.world.entity.player.Player;
import net.neoforged.bus.api.Event;
import net.neoforged.bus.api.ICancellableEvent;


import javax.annotation.Nullable;

public abstract class RegionEvent extends Event {

    private final IMarkableRegion region;
    @Nullable
    private final Player player;

    private RegionEvent(IMarkableRegion region, Player player) {
        this.region = region;
        this.player = player;
    }

    public IMarkableRegion getRegion() {
        return region;
    }

    public Player getPlayer() {
        return player;
    }


    /**
     * This event is fired whenever a new region is created. This event is cancelable.
     * When this is event is canceled, the region will not be created.
     */
    public static class CreateRegionEvent extends RegionEvent implements ICancellableEvent {

        public CreateRegionEvent(IMarkableRegion region, Player player) {
            super(region, player);
        }

    }

    /**
     * This event is fired whenever a region is about to be removed. This event is cancelable.
     * When this is event is canceled, the region will not be deleted.
     */
    public static class RemoveRegionEvent extends RegionEvent  implements ICancellableEvent{

        public RemoveRegionEvent(IMarkableRegion region, Player player) {
            super(region, player);
        }
    }

    /**
     * This event is fired whenever a region is updated. This event is cancelable.
     * When this is event is canceled, the region properties will not be changed.
     */
    public static class UpdateRegionEvent extends RegionEvent  implements ICancellableEvent{

        public UpdateRegionEvent(IMarkableRegion region, Player player) {
            super(region, player);
        }
    }
}


