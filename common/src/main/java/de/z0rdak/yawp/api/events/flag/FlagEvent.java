package de.z0rdak.yawp.api.events.flag;

import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.server.level.ServerPlayer;
import org.jetbrains.annotations.Nullable;

public abstract class FlagEvent {

    private final IProtectedRegion region;
    private final IFlag flag;
    private final ServerPlayer player;

    private FlagEvent(final ServerPlayer player, final IProtectedRegion region, final IFlag flag) {
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

    public static class Add extends FlagEvent {

        public Add(final ServerPlayer player, final IProtectedRegion region, final IFlag flag) {
            super(player, region, flag);
        }

        @Override
        @Nullable
        public ServerPlayer getPlayer() {
            return super.getPlayer();
        }
    }

    public static class Remove extends FlagEvent {

        public Remove(final ServerPlayer player, final IProtectedRegion region, final IFlag flag) {
            super(player, region, flag);
        }

        @Override
        @Nullable
        public ServerPlayer getPlayer() {
            return super.getPlayer();
        }
    }

    public static class UpdateFlagMessage extends FlagEvent {

        private String newMsg;

        public UpdateFlagMessage(final ServerPlayer player, final IProtectedRegion region, final IFlag flag, String newMsg) {
            super(player, region, flag);
            this.newMsg = newMsg;
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
    


