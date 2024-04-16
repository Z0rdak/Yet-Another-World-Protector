package de.z0rdak.yawp.api.events.region;

import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.world.entity.player.Player;
import net.minecraftforge.eventbus.api.Cancelable;
import net.minecraftforge.eventbus.api.Event;

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
    @Cancelable
    public static class CreateRegionEvent extends RegionEvent {

        public CreateRegionEvent(IMarkableRegion region, Player player) {
            super(region, player);
        }

    }

    /**
     * This event is fired whenever a new region is renamed. This event is cancelable.
     * When this is event is canceled, the region will not be renamed.
     */
    @Cancelable
    public static class RenameRegion extends RegionEvent {

        private final String oldName;
        private String newName;

        public RenameRegion(IMarkableRegion region, String oldName, String newName, PlayerEntity player) {
            super(region, player);
            this.newName = newName;
            this.oldName = oldName;
        }

        public String getOldName() {
            return oldName;
        }

        public String getNewName() {
            return newName;
        }

        public void setNewName(String newName) {
            this.newName = newName;
        }
    }

    /**
     * This event is fired whenever a new area is created. This event is cancelable.
     * Canceling this event will prevent the area from being updated.
     */
    @Cancelable
    public static class UpdateArea extends RegionEvent {

        private IMarkableArea markedArea;

        public UpdateArea(IMarkableRegion region, IMarkableArea area, PlayerEntity player) {
            super(region, player);
            this.markedArea = area;
        }

        public IMarkableArea getMarkedArea() {
            return markedArea;
        }

        public void setMarkedArea(IMarkableArea markedArea) {
            this.markedArea = markedArea;
        }
    }


    /**
     * This event is fired whenever a region is about to be removed. This event is cancelable.
     * When this is event is canceled, the region will not be deleted.
     */
    @Cancelable
    public static class RemoveRegionEvent extends RegionEvent {

        public RemoveRegionEvent(IMarkableRegion region, Player player) {
            super(region, player);
        }
    }
}


