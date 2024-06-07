package de.z0rdak.yawp.util;

import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.core.flag.FlagMessage;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.text.MutableText;
import net.minecraft.text.Text;
import org.apache.logging.log4j.core.jmx.Server;

import java.util.Map;

import static de.z0rdak.yawp.core.flag.FlagMessage.REGION_TEMPLATE;

public class MessageSender {

    public static void sendCmdFeedback(ServerCommandSource src, MutableText text) {
        try {
            if (src.getEntity() == null) {
                src.sendFeedback(() -> text, true);
            } else {
                sendMessage(src.getPlayerOrThrow(), text);
            }
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.error(e);
        }
    }

    public static void sendCmdFeedback(ServerCommandSource src, String langKey) {
        sendCmdFeedback(src, Text.translatable(langKey));
    }

    public static void sendCmdFeedback(ServerCommandSource src, String langKey, String fallback) {
        sendCmdFeedback(src, Text.translatableWithFallback(langKey, fallback));
    }

    public static void sendDimFlagNotification(PlayerEntity player, RegionFlag flag) {
        player.sendMessage(Text.translatableWithFallback("flag.dim.player.msg.push.deny", "The '%s' flag denies this action in this dimension!", flag.name), true);
    }

    public static void sendFlagNotification(PlayerEntity player, IMarkableRegion region, RegionFlag flag) {
        player.sendMessage(Text.translatableWithFallback("flag.local.player.msg.push.deny", "[%s]: The '%s' flag denies this action here!", region.getName(), flag.name), true);
    }

    public static void sendCmdFeedback(CommandContext<ServerCommandSource> src, String langKey) {
        sendCmdFeedback(src.getSource(), Text.translatable(langKey));
    }

    public static void sendCmdFeedback(CommandContext<ServerCommandSource> src, String langKey, String fallback) {
        sendCmdFeedback(src.getSource(), Text.translatableWithFallback(langKey, fallback));
    }

    public static void sendMessage(PlayerEntity player, String translationKey) {
        player.sendMessage(Text.translatable(translationKey));
    }

    public static void sendMessage(PlayerEntity player, String translationKey, String fallback) {
        player.sendMessage(Text.translatableWithFallback(translationKey, fallback));
    }

    public static void sendMessage(PlayerEntity player, MutableText textComponent) {
        player.sendMessage(textComponent);
    }

    public static void sendNotification(PlayerEntity player, MutableText msg) {
        player.sendMessage(msg, true);
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
            MutableText flagMsg = FlagMessage.buildFrom(result, msgSubstitutes);
            sendNotification(player, flagMsg);
        }
    }
}
