package de.z0rdak.yawp.api.events.region;

import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import org.jetbrains.annotations.Nullable;

public abstract class RegionEvent {

    private final IMarkableRegion region;
    @Nullable
    private final Player player;

    private RegionEvent(IMarkableRegion region, @Nullable Player player) {
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
    public final static class Create extends RegionEvent {

        public Create(IMarkableRegion region, Player player) {
            super(region, player);
        }

    }

    /**
     * This event is fired whenever a new region is renamed. This event is cancelable.
     * When this is event is canceled, the region will not be renamed.
     */
    public final static class Rename extends RegionEvent {

        private final String oldName;
        private String newName;

        public Rename(IMarkableRegion region, String oldName, String newName, Player player) {
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
    public final static class UpdateArea extends RegionEvent {

        private IMarkableArea markedArea;

        public UpdateArea(IMarkableRegion region, IMarkableArea area, Player player) {
            super(region, player);
            this.markedArea = area;
        }

        public IMarkableArea markedArea() {
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
    public final static class Remove extends RegionEvent {

        public Remove(IMarkableRegion region, Player player) {
            super(region, player);
        }
    }
}


