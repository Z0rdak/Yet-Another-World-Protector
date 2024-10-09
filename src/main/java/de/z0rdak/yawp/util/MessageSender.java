package de.z0rdak.yawp.util;

import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.core.flag.FlagMessage;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.world.entity.player.Player;

import java.util.Map;

import static de.z0rdak.yawp.core.flag.FlagMessage.REGION_TEMPLATE;

public class MessageSender {
    public static void sendCmdFeedback(CommandSourceStack src, MutableComponent text) {
        src.sendSystemMessage(text);
    }

    public static void sendCmdFeedback(CommandSourceStack src, String langKey) {
        sendCmdFeedback(src, Component.translatable(langKey));
    }

    public static void sendCmdFeedback(CommandSourceStack src, String langKey, String fallback) {
        sendCmdFeedback(src, Component.translatableWithFallback(langKey, fallback));
    }

    public static void sendMessage(Player player, String translationKey) {
        player.sendSystemMessage(Component.translatable(translationKey));
    }

    public static void sendMessage(Player player, String translationKey, String fallback) {
        player.sendSystemMessage(Component.translatableWithFallback(translationKey, fallback));
    }

    public static void sendMessage(Player player, MutableComponent textComponent) {
        player.sendSystemMessage(textComponent);
    }

    public static void sendNotification(Player player, MutableComponent msg) {
        player.displayClientMessage(msg, true);
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
        IFlag flag = responsibleRegion.getFlag(result.getFlagCheck().getRegionFlag().name);
        if (flag == null || result.getFlagState() == FlagState.UNDEFINED || result.getFlagState() == FlagState.DISABLED) {
            return;
        }
        boolean isFlagMuted = flag.getFlagMsg().isMuted() || responsibleRegion.isMuted();
        Player player = result.getFlagCheck().getPlayer();
        // If not muted and the event is a player event, send the message
        if (!isFlagMuted && player instanceof Player) {
            Map<String, String> msgSubstitutes = FlagMessage.defaultSubstitutesFor(result);


            msgSubstitutes.put(REGION_TEMPLATE, responsibleRegion.getName());
            MutableComponent flagMsg = FlagMessage.buildFrom(result, msgSubstitutes);
            sendNotification(player, flagMsg);
        }
    }
}
