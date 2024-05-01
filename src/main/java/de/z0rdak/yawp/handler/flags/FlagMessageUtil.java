package de.z0rdak.yawp.handler.flags;

import de.z0rdak.yawp.core.flag.FlagCategory;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.entity.player.Player;

import javax.annotation.Nullable;
import java.util.HashMap;
import java.util.Map;

import static de.z0rdak.yawp.core.flag.FlagCategory.PLAYER;

public final class FlagMessageUtil {

    public static final String FLAG_TEMPLATE = "{flag}";
    public static final String POS_TEMPLATE = "{pos}";
    public static final String REGION_TEMPLATE = "{region}";
    public static final String DIM_TEMPLATE = "{dim}";
    public static final String PLAYER_TEMPLATE = "{player}";
    private FlagMessageUtil() {
    }

    /**
     * Returns a map with default substitutes for the given flag and region. <br>
     *
     * @param flag   the flag to get the substitutes for
     * @param region the region to get the substitutes for
     * @param pos    the position to get the substitutes for
     * @param player the player to get the substitutes for
     * @return a map with default substitutes for the given flag and region
     */
    public static Map<String, String> defaultSubstitutes(RegionFlag flag, IProtectedRegion region, BlockPos pos, @Nullable Player player) {
        Map<String, String> substituteMap = new HashMap<>();
        substituteMap.put(FLAG_TEMPLATE, flag.name);
        substituteMap.put(POS_TEMPLATE, MessageUtil.shortBlockPos(pos));
        substituteMap.put(REGION_TEMPLATE, region.getName());
        substituteMap.put(DIM_TEMPLATE, region.getDim().location().toString());
        if (player != null && flag.categories.contains(PLAYER)) {
            substituteMap.put(PLAYER_TEMPLATE, player.getScoreboardName());
        }
        return substituteMap;
    }

    /**
     * Sends the flag message for the given flag check event. <br>
     *
     * @param flagCheck the flag check event to send the message for
     */
    public static void sendFlagMsg(FlagCheckEvent flagCheck) {
        IProtectedRegion responsibleRegion = flagCheck.getResponsibleRegion();
        if (responsibleRegion == null) {
            return;
        }
        IFlag flag = responsibleRegion.getFlag(flagCheck.getRegionFlag().name);
        boolean isFlagMuted = flag.getFlagMsg().isMuted();
        // If not muted and the event is a player event, send the message
        if (!isFlagMuted && flagCheck instanceof PlayerFlagEvent) {
            PlayerFlagEvent playerFlagEvent = (PlayerFlagEvent) flagCheck;
            playerFlagEvent.getMsgSubstitutes().put(REGION_TEMPLATE, responsibleRegion.getName());
            MutableComponent flagMsg = buildFlagMsg(flag, playerFlagEvent);
            MessageUtil.sendNotification(playerFlagEvent.getPlayer(), flagMsg);
        }
    }


    /**
     * Builds the flag message for the given flag check event and flag. <br>
     * If the flag has a custom message defined, that message is returned instead, else the I18n message is returned. <br>
     *
     * @param flagCheckEvent the flag check event to build the message for
     * @param flag           the flag to build the message for
     * @return the flag message for the given flag check event and flag
     */
    public static MutableComponent buildFlagMsg(IFlag flag, PlayerFlagEvent flagCheckEvent) {
        String flagMsgTemplate = flag.getFlagMsg().isDefault()
                ? getI18nFlagMsgTemplate(flag)
                : flag.getFlagMsg().getMsg();
        String flagMsg = replaceMatches(flagMsgTemplate, flagCheckEvent);
        return new TextComponent(flagMsg);
    }

    /**
     * Returns the flag message template for the given flag from the I18n keys. <br>
     * If the flag has a custom message defined, that message is returned instead. <br>
     *
     * @param flag the flag to get the default message template for
     * @return the default flag message template for the given flag
     */
    private static String getI18nFlagMsgTemplate(IFlag flag) {
        RegionFlag regionFlag = RegionFlag.fromId(flag.getName());
        String flagMsgLangKey = regionFlag.categories.contains(FlagCategory.PLAYER) ? "flag.msg.deny." + flag.getName() : "flag.msg.deny.default";
        return new TranslatableComponent(flagMsgLangKey).getString();
    }

    /**
     * Replaces the matches in the given flag message template with the substitutes from the given flag check event. <br>
     *
     * @param flagMsgTemplate the flag message template to replace the matches in
     * @param flagCheckEvent  the flag check event to get the substitutes from
     * @return the flag message with the matches replaced
     */
    private static String replaceMatches(String flagMsgTemplate, PlayerFlagEvent flagCheckEvent) {
        String flagMsg = flagMsgTemplate;
        for (Map.Entry<String, String> entry : flagCheckEvent.getMsgSubstitutes().entrySet()) {
            flagMsg = flagMsg.replace(entry.getKey(), entry.getValue());
        }
        return flagMsg;
    }
}
