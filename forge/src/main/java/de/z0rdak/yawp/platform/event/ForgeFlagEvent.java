package de.z0rdak.yawp.platform.event;

import de.z0rdak.yawp.api.events.flag.FlagEvent;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.eventbus.api.Event;
import org.jetbrains.annotations.Nullable;

public abstract class ForgeFlagEvent extends Event {

    private final IProtectedRegion region;
    private final IFlag flag;
    private final ServerPlayer player;

    private ForgeFlagEvent(final ServerPlayer player, final IProtectedRegion region, final IFlag flag) {
        this.player = player;
        this.region = region;
        this.flag = flag;
    }

    public ServerPlayer getPlayer() {
        return player;
    }

    public IFlag getFlag() {
        return flag;
    }

    public IProtectedRegion getRegion() {
        return region;
    }

    public static class Add extends ForgeFlagEvent {

        public Add(final ServerPlayer player, final IProtectedRegion region, final IFlag flag) {
            super(player, region, flag);
        }

        @Override
        @Nullable
        public ServerPlayer getPlayer() {
            return super.getPlayer();
        }

        public Add(FlagEvent.Add event) {
            super(event.getPlayer(), event.getRegion(), event.getFlag());
        }
    }

    public static class Remove extends ForgeFlagEvent {

        public Remove(final ServerPlayer player, final IProtectedRegion region, final IFlag flag) {
            super(player, region, flag);
        }

        public Remove(FlagEvent.Remove event) {
            super(event.getPlayer(), event.getRegion(), event.getFlag());
        }

        @Override
        @Nullable
        public ServerPlayer getPlayer() {
            return super.getPlayer();
        }
    }

    public static class UpdateFlagMessage extends ForgeFlagEvent {

        private String newMsg;

        public UpdateFlagMessage(final ServerPlayer player, final IProtectedRegion region, final IFlag flag, String newMsg) {
            super(player, region, flag);
            this.newMsg = newMsg;
        }

        public UpdateFlagMessage(FlagEvent.UpdateFlagMessage event) {
            this(event.getPlayer(), event.getRegion(), event.getFlag(), event.getNewMsg());
        }

        @Override
        @Nullable
        public ServerPlayer getPlayer() {
            return super.getPlayer();
        }

        public String getNewMsg() {
            return this.newMsg;
        }

        public void setNewMsg(String newMsg) {
            this.newMsg = newMsg;
        }
    }
}
    


