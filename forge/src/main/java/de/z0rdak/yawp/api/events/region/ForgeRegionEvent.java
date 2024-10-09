package de.z0rdak.yawp.api.events.region;

import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraftforge.eventbus.api.Cancelable;
import net.minecraftforge.eventbus.api.Event;

import javax.annotation.Nullable;

public abstract class ForgeRegionEvent extends Event {

    private final IMarkableRegion region;
    @Nullable
    private final Player player;

    private ForgeRegionEvent(IMarkableRegion region, @Nullable Player player) {
        this.region = region;
        this.player = player;
    }

    public IMarkableRegion getRegion() {
        return region;
    }

    @Nullable
    public Player getPlayer() {
        return player;
    }

    /**
     * This event is fired whenever a new region is created. This event is cancelable.
     * When this is event is canceled, the region will not be created.
     */
    @Cancelable
    public static class Create extends ForgeRegionEvent {

        public Create(IMarkableRegion region, Player player) {
            super(region, player);
        }

        public Create(RegionEvent.Create event) {
            this(event.getRegion(), event.getPlayer());
        }

        public static RegionEvent.Create asCommonEvent(Create event) {
            return new RegionEvent.Create(event.getRegion(), event.getPlayer());
        }
    }

    /**
     * This event is fired whenever a new region is renamed. This event is cancelable.
     * When this is event is canceled, the region will not be renamed.
     */
    @Cancelable
    public static class Rename extends ForgeRegionEvent {

        private final String oldName;
        private String newName;

        public Rename(IMarkableRegion region, String oldName, String newName, Player player) {
            super(region, player);
            this.newName = newName;
            this.oldName = oldName;
        }

        public Rename(RegionEvent.Rename event) {
            this(event.getRegion(), event.getOldName(), event.getNewName(), event.getPlayer());
        }

        public static RegionEvent.Rename asCommonEvent(Rename event) {
            return new RegionEvent.Rename(event.getRegion(), event.getOldName(), event.getNewName(), event.getPlayer());
        }

        public String getOldName() {
            return oldName;
        }

        public String getNewName() {
            return newName;
        }

        /**
         * The name set here is not validated again. Be sure you validate the name before setting it. <br>
         * Otherwise, you may cause inconsistencies and break your whole region definition.
         *
         * @param newName The new name of the region - be sure to validate it before
         * @see de.z0rdak.yawp.data.region.RegionDataManager#isValidRegionName(ResourceKey, String)
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
    public static class UpdateArea extends ForgeRegionEvent {

        private IMarkableArea markedArea;

        public UpdateArea(IMarkableRegion region, IMarkableArea area, Player player) {
            super(region, player);
            this.markedArea = area;
        }

        public UpdateArea(RegionEvent.UpdateArea event) {
            this(event.getRegion(), event.markedArea(), event.getPlayer());
        }

        public static RegionEvent.UpdateArea asCommonEvent(ForgeRegionEvent.UpdateArea event) {
            return new RegionEvent.UpdateArea(event.getRegion(), event.getMarkedArea(), event.getPlayer());
        }

        public IMarkableArea getMarkedArea() {
            return markedArea;
        }

        /**
         * The area set here is not validated again. Be sure you validate the area before setting it. <br>
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
    public static class Remove extends ForgeRegionEvent {

        public Remove(IMarkableRegion region, Player player) {
            super(region, player);
        }

        public Remove(RegionEvent.Remove event) {
            this(event.getRegion(), event.getPlayer());
        }

        public static RegionEvent.Remove asCommonEvent(Remove event) {
            return new RegionEvent.Remove(event.getRegion(), event.getPlayer());
        }
    }
}


