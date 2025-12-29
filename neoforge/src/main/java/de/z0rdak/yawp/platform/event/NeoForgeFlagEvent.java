package de.z0rdak.yawp.platform.event;

import de.z0rdak.yawp.api.events.flag.FlagEvent;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.server.level.ServerPlayer;
import net.neoforged.bus.api.Event;
import net.neoforged.bus.api.ICancellableEvent;

public abstract class NeoForgeFlagEvent extends Event {

    private final IProtectedRegion region;
    private final IFlag flag;
    private final ServerPlayer player;

    private NeoForgeFlagEvent(final ServerPlayer player, final IProtectedRegion region, final IFlag flag) {
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

    public static class Add extends NeoForgeFlagEvent implements ICancellableEvent {

        public Add(final ServerPlayer player, final IProtectedRegion region, final IFlag flag) {
            super(player, region, flag);
        }

        public Add(FlagEvent.Add event) {
            super(event.getPlayer(), event.getRegion(), event.getFlag());
        }
    }

    public static class Remove extends NeoForgeFlagEvent implements ICancellableEvent {

        public Remove(final ServerPlayer player, final IProtectedRegion region, final IFlag flag) {
            super(player, region, flag);
        }

        public Remove(FlagEvent.Remove event) {
            super(event.getPlayer(), event.getRegion(), event.getFlag());
        }
    }

    public static class UpdateFlagMessage extends NeoForgeFlagEvent implements ICancellableEvent {

        private String newMsg;

        public UpdateFlagMessage(final ServerPlayer player, final IProtectedRegion region, final IFlag flag, String newMsg) {
            super(player, region, flag);
            this.newMsg = newMsg;
        }

        public UpdateFlagMessage(FlagEvent.UpdateFlagMessage event) {
            this(event.getPlayer(), event.getRegion(), event.getFlag(), event.getNewMsg());
        }

        public static FlagEvent.UpdateFlagMessage asNonEvent(FlagEvent.UpdateFlagMessage forgeEvent) {
            return new FlagEvent.UpdateFlagMessage(forgeEvent.getPlayer(), forgeEvent.getRegion(), forgeEvent.getFlag(), forgeEvent.getNewMsg());
        }

        public static FlagEvent.UpdateFlagMessage asEvent(FlagEvent.UpdateFlagMessage event) {
            return new FlagEvent.UpdateFlagMessage(event.getPlayer(), event.getRegion(), event.getFlag(), event.getNewMsg());
        }

        public String getNewMsg() {
            return this.newMsg;
        }

        public void setNewMsg(String newMsg) {
            this.newMsg = newMsg;
        }
    }
}
    


