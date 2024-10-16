package de.z0rdak.yawp.api.events.region;

import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.RegistryKey;
import net.minecraftforge.eventbus.api.Cancelable;
import net.minecraftforge.eventbus.api.Event;

import javax.annotation.Nullable;

public abstract class RegionEvent extends Event {

    private final IMarkableRegion region;
    @Nullable
    private final PlayerEntity player;

    private RegionEvent(IMarkableRegion region, @Nullable PlayerEntity player) {
        this.region = region;
        this.player = player;
    }

    public IMarkableRegion getRegion() {
        return region;
    }

    @Nullable
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

        /**
         * The name set here is not validated again. Be sure you validate the name before setting it. <br></br>
         * Otherwise, you may cause inconsistencies and break your whole region definition.
         *
         * @param newName The new name of the region - be sure to validate it before
         * @see de.z0rdak.yawp.managers.data.region.RegionDataManager#isValidRegionName(RegistryKey, String) 
         */
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

        /**
         * The area set here is not validated again. Be sure you validate the area before setting it. <br></br>
         * Otherwise, you may cause inconsistencies and break your whole region definition.
         *
         * @param markedArea The new area of the region - be sure to validate it before
         */
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

        public RemoveRegionEvent(IMarkableRegion region, PlayerEntity player) {
            super(region, player);
        }
    }
}


