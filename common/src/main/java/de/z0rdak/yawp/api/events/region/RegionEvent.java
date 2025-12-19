package de.z0rdak.yawp.api.events.region;

import de.z0rdak.yawp.api.events.Cancelable;
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

    public IMarkableRegion getRegion() { return region; }
    public ServerPlayer getPlayer() { return player; }

    public final static class Create extends RegionEvent implements Cancelable {
        private boolean canceled;

        public Create(final IMarkableRegion region, final ServerPlayer player) { super(region, player); }

        @Override
        @Nullable
        public ServerPlayer getPlayer() { return super.getPlayer(); }

        @Override
        public boolean isCanceled() { return canceled; }
        @Override
        public void setCanceled(boolean canceled) { this.canceled = canceled; }
    }

    public final static class Rename extends RegionEvent implements Cancelable {
        private final String oldName;
        private String newName;
        private boolean canceled;

        public Rename(final IMarkableRegion region, final String oldName, final String newName, final ServerPlayer player) {
            super(region, player);
            this.oldName = oldName;
            this.newName = newName;
        }

        @Override
        @Nullable
        public ServerPlayer getPlayer() { return super.getPlayer(); }

        public String getOldName() { return oldName; }
        public String getNewName() { return newName; }
        public void setNewName(String newName) { this.newName = newName; }

        @Override
        public boolean isCanceled() { return canceled; }
        @Override
        public void setCanceled(boolean canceled) { this.canceled = canceled; }
    }

    public final static class UpdateArea extends RegionEvent implements Cancelable {
        private IMarkableArea markedArea;
        private boolean canceled;

        public UpdateArea(final IMarkableRegion region, final IMarkableArea area, final ServerPlayer player) {
            super(region, player);
            this.markedArea = area;
        }

        @Override
        @Nullable
        public ServerPlayer getPlayer() { return super.getPlayer(); }

        public IMarkableArea markedArea() { return markedArea; }
        public void setMarkedArea(IMarkableArea markedArea) { this.markedArea = markedArea; }

        @Override
        public boolean isCanceled() { return canceled; }
        @Override
        public void setCanceled(boolean canceled) { this.canceled = canceled; }
    }

    public final static class Remove extends RegionEvent implements Cancelable {
        private boolean canceled;

        public Remove(final IMarkableRegion region, final ServerPlayer player) { super(region, player); }

        @Override
        @Nullable
        public ServerPlayer getPlayer() { return super.getPlayer(); }

        @Override
        public boolean isCanceled() { return canceled; }
        @Override
        public void setCanceled(boolean canceled) { this.canceled = canceled; }
    }

    public static abstract class PlayerMove extends RegionEvent {
        private final BlockPos previousPos;
        private final BlockPos currentPos;

        public PlayerMove(final IMarkableRegion region, final ServerPlayer player, final BlockPos previousPos, final BlockPos currentPos) {
            super(region, player);
            this.previousPos = previousPos;
            this.currentPos = currentPos;
        }

        public BlockPos previous() { return previousPos; }
        public BlockPos current() { return currentPos; }

    }

    public final static class PlayerEnter extends PlayerMove implements Cancelable {
        public PlayerEnter(final IMarkableRegion region, final ServerPlayer player, final BlockPos oldPos, final BlockPos newPos) {
            super(region, player, oldPos, newPos);
        }
        private boolean canceled;

        @Override public boolean isCanceled() { return canceled; }
        @Override public void setCanceled(boolean canceled) { this.canceled = canceled; }
    }

    public final static class PlayerLeave extends PlayerMove implements Cancelable {
        public PlayerLeave(final IMarkableRegion region, final ServerPlayer player, final BlockPos oldPos, final BlockPos newPos) {
            super(region, player, oldPos, newPos);
        }
        private boolean canceled;
        @Override public boolean isCanceled() { return canceled; }
        @Override public void setCanceled(boolean canceled) { this.canceled = canceled; }
    }
}
