package de.z0rdak.yawp.api;

import de.z0rdak.yawp.api.events.flag.FlagCheckResult;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;

import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.util.text.messages.FlagMessageBuilder;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.world.entity.player.Player;

public class MessageSender {

    public static void sendCmdFeedback(CommandSourceStack src, MutableComponent text) {
        src.sendSystemMessage(text);
    }

    public static void message(Player player, MutableComponent msg) {
        player.sendSystemMessage(msg);
    }

    public static void message(Player player, String msg) {
        player.sendSystemMessage(Component.literal(msg));
    }

    public static void overLayMessage(Player player, String msg) {
        player.sendOverlayMessage(Component.literal(msg));
    }

    public static void overLayMessage(Player player, MutableComponent msg) {
        player.sendOverlayMessage(msg);
    }

    public static void sendError(CommandSourceStack src, MutableComponent text) {
        src.sendFailure(text);
    }

    public static void sendNotification(Player player, MutableComponent msg) {
        player.sendOverlayMessage(msg);
    }

    /**
     * Sends the flag message for the given flag check event. <br>     *
     *
     * @param result the flag check event to send the message for
     */
    public static void sendFlagMsg(FlagCheckResult result) {
        IProtectedRegion responsibleRegion = result.getResponsible();
        if (responsibleRegion == null) {
            return;
        }
        IFlag flag = responsibleRegion.getFlag(result.getFlagCheck().getRegionFlag().name());
        if (flag == null || result.getFlagState() == FlagState.UNDEFINED || result.getFlagState() == FlagState.DISABLED) {
            return;
        }
        boolean isFlagMuted = flag.getFlagMsg().isMuted() || responsibleRegion.isMuted();
        Player player = result.getFlagCheck().getPlayer();
        // If not muted and the event is a player event, and the player is not null, send the message
        if (!isFlagMuted && FlagRegister.hasPlayerTag(flag) && player != null) {
            MutableComponent flagMsg = FlagMessageBuilder.buildFrom(result, null);
            sendNotification(player, flagMsg);
        }
    }
}
