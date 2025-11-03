package de.z0rdak.yawp.api.events.region;

import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerPlayer;
import org.jetbrains.annotations.Nullable;

public abstract class RegionEvent {

    private final IMarkableRegion region;
    private final ServerPlayer player;

    private RegionEvent(final IMarkableRegion region, final ServerPlayer player) {
        this.region = region;
        this.player = player;
    }

    public IMarkableRegion getRegion() {
        return region;
    }

    public ServerPlayer getPlayer() {
        return player;
    }


    /**
     * This event is fired whenever a new region is created. This event is cancelable.
     * When this is event is canceled, the region will not be created.
     */
    public final static class Create extends RegionEvent {

        public Create(final IMarkableRegion region, final ServerPlayer player) {
            super(region, player);
        }

        @Override
        @Nullable
        public ServerPlayer getPlayer() {
            return super.getPlayer();
        }
    }

    /**
     * This event is fired whenever a new region is renamed. This event is cancelable.
     * When this is event is canceled, the region will not be renamed.
     */
    public final static class Rename extends RegionEvent {

        private final String oldName;
        private String newName;

        public Rename(final IMarkableRegion region, final String oldName, final String newName, final ServerPlayer player) {
            super(region, player);
            this.newName = newName;
            this.oldName = oldName;
        }

        @Override
        @Nullable
        public ServerPlayer getPlayer() {
            return super.getPlayer();
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
         * @see de.z0rdak.yawp.data.region.LevelRegionData#isValidRegionName(String)
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

        public UpdateArea(final IMarkableRegion region, final IMarkableArea area, final ServerPlayer player) {
            super(region, player);
            this.markedArea = area;
        }

        @Override
        @Nullable
        public ServerPlayer getPlayer() {
            return super.getPlayer();
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

        @Override
        @Nullable
        public ServerPlayer getPlayer() {
            return super.getPlayer();
        }

        public Remove(final IMarkableRegion region, final ServerPlayer player) {
            super(region, player);
        }
    }


    public static abstract class PlayerMove extends RegionEvent {
        private final BlockPos previousPos;
        private final BlockPos currentPos;
        public PlayerMove(final IMarkableRegion region, final ServerPlayer player, final BlockPos previousPos, final BlockPos currentPos) {
            super(region, player);
            this.previousPos = previousPos;
            this.currentPos = currentPos;
        }

        public BlockPos previous() {
            return previousPos;
        }

        public BlockPos current() {
            return currentPos;
        }
    }

    public final static class PlayerEnter extends PlayerMove {
        public PlayerEnter(final IMarkableRegion region, final ServerPlayer player, final BlockPos oldPos, final BlockPos newPos) {
            super(region, player, oldPos, newPos);
        }

    }

    public final static class PlayerLeave extends PlayerMove {
        public PlayerLeave(final IMarkableRegion region, final ServerPlayer player, final BlockPos oldPos, final BlockPos newPos) {
            super(region, player, oldPos, newPos);
        }
    }
}


