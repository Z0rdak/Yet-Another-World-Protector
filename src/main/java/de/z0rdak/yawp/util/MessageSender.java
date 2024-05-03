package de.z0rdak.yawp.util;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.core.flag.FlagMessage;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.command.CommandSource;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.text.IFormattableTextComponent;
import net.minecraft.util.text.TranslationTextComponent;

import java.util.Map;

import static de.z0rdak.yawp.core.flag.FlagMessage.REGION_TEMPLATE;

public class MessageSender {
    public static void sendCmdFeedback(CommandSource src, IFormattableTextComponent text) {
        try {
            if (src.getEntity() == null) {
                src.sendSuccess(text, true);
            } else {
                sendMessage(src.getPlayerOrException(), text);
            }
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.error(e);
        }
    }

    public static void sendCmdFeedback(CommandSource src, String langKey) {
        sendCmdFeedback(src, new TranslationTextComponent(langKey));
    }

    public static void sendMessage(PlayerEntity player, String translationKey) {
        player.sendMessage(new TranslationTextComponent(translationKey), player.getUUID());
    }

    public static void sendMessage(PlayerEntity player, IFormattableTextComponent textComponent) {
        player.sendMessage(textComponent, player.getUUID());
    }

    public static void sendNotification(PlayerEntity player, IFormattableTextComponent msg) {
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
        PlayerEntity player = result.getFlagCheck().getPlayer();
        // If not muted and the event is a player event, send the message
        if (!isFlagMuted && player instanceof PlayerEntity) {
            Map<String, String> msgSubstitutes = FlagMessage.defaultSubstitutesFor(result);
            msgSubstitutes.put(REGION_TEMPLATE, responsibleRegion.getName());
            IFormattableTextComponent flagMsg = FlagMessage.buildFrom(result, msgSubstitutes);
            sendNotification(player, flagMsg);
        }
    }
}
