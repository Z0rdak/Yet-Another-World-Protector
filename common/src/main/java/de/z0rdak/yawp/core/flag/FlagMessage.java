package de.z0rdak.yawp.core.flag;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.api.FlagTagRegister;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.world.entity.player.Player;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.Map.Entry;

import static de.z0rdak.yawp.util.ChatComponentBuilder.*;

public class FlagMessage {

    public static final String FLAG_TEMPLATE = "{flag}";
    public static final String POS_TEMPLATE = "{pos}";
    public static final String REGION_TEMPLATE = "{region}";
    public static final String DIM_TEMPLATE = "{dim}";
    public static final String PLAYER_TEMPLATE = "{player}";
    public static final String TEAM_TEMPLATE = "{team}";
    public static final String GROUP_TEMPLATE = "{group}";
    public static final String ENTITY_TEMPLATE = "{entity}";
    public static final String BLOCK_ENTITY = "{block}";

    public static final String CONFIG_MSG = "config";
    public static final Set<String> MSG_TOKEN;
    public static FlagMessage DEFAULT_FLAG_MSG = new FlagMessage(CONFIG_MSG);

    static {
        MSG_TOKEN = new HashSet<>();
        MSG_TOKEN.add(FLAG_TEMPLATE);
        MSG_TOKEN.add(REGION_TEMPLATE);
        MSG_TOKEN.add(DIM_TEMPLATE);
        MSG_TOKEN.add(POS_TEMPLATE);
        MSG_TOKEN.add(PLAYER_TEMPLATE);
        MSG_TOKEN.add(TEAM_TEMPLATE);
        MSG_TOKEN.add(GROUP_TEMPLATE);
        MSG_TOKEN.add(ENTITY_TEMPLATE);
        MSG_TOKEN.add(BLOCK_ENTITY);
    }

    private static final Map<String, Integer> TOKEN_INDEX;
    static {

        TOKEN_INDEX = Map.of(
                FLAG_TEMPLATE, 1,   // %1$s
                REGION_TEMPLATE, 2, // %2$s
                DIM_TEMPLATE, 3,    // %3$s
                POS_TEMPLATE, 4,    // %4$s
                PLAYER_TEMPLATE, 5, // %5$s
                TEAM_TEMPLATE, 6,   // %6$s
                GROUP_TEMPLATE, 7,  // %7$s
                ENTITY_TEMPLATE, 8, // %8$s
                BLOCK_ENTITY, 9);   // %9$s
    }


    public static Codec<FlagMessage> CODEC = RecordCodecBuilder.create(
            instance -> instance.group(
                    Codec.STRING.fieldOf("msg")
                            .orElse(FlagMessage.CONFIG_MSG)
                            .forGetter(FlagMessage::msg),
                    Codec.BOOL.fieldOf("muted")
                            .orElse(false)
                            .forGetter(FlagMessage::isMuted),
                    Codec.BOOL.fieldOf("default")
                            .orElse(true)
                            .forGetter(FlagMessage::isDefault)
            ).apply(instance, FlagMessage::new));

    private String msg;
    private boolean muted;
    private boolean isDefault;

    public FlagMessage(String msg) {
        this.msg = msg;
        this.isDefault = msg.toLowerCase(Locale.ROOT).equals(CONFIG_MSG);
    }

    public FlagMessage(String msg, boolean muted) {
        this(msg);
        this.muted = muted;
    }

    public FlagMessage(String msg, boolean muted, boolean isDefault) {
        this(msg, muted);
        this.isDefault = isDefault;
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
        substituteMap.put(POS_TEMPLATE, shortBlockPosBracketed(pos));
        substituteMap.put(REGION_TEMPLATE, region.getName());
        substituteMap.put(DIM_TEMPLATE, region.getDim().identifier().toString());
        if (player != null && flag.tags.contains(FlagTagRegister.PLAYER)) {
            substituteMap.put(PLAYER_TEMPLATE, player.getScoreboardName());
        }
        return substituteMap;
    }

    /**
     * Returns a map with default substitutes for the given flag check result. <br>
     * The substitutes are built from the flag, responsible region, position and player in the result. <br>
     * The substitutes are then returned as a map. <br>
     *
     * @param result the flag check result to get the default substitutes for
     * @return a map with default substitutes for the given flag check result
     */
    public static Map<String, String> defaultSubstitutesFor(FlagCheckResult result) {
        Map<String, String> substituteMap = new HashMap<>();
        substituteMap.put(FLAG_TEMPLATE, result.getFlagCheck().getRegionFlag().name);
        substituteMap.put(POS_TEMPLATE, tinyBlockPos(result.getFlagCheck().getTarget()));
        substituteMap.put(REGION_TEMPLATE, result.getResponsible().getName());
        substituteMap.put(DIM_TEMPLATE, result.getResponsible().getDim().identifier().toString());
        if (result.getFlagCheck().getPlayer() != null && RegionFlag.hasPlayerCategory(result.getFlagCheck().getRegionFlag())) {
            substituteMap.put(PLAYER_TEMPLATE, result.getFlagCheck().getPlayer().getScoreboardName());
        }
        return substituteMap;
    }

    /**
     * Builds a flag message from the given flag check result and substitutes. <br>
     * The flag message is built from the flag message template of the flag in the result. <br>
     * The matches in the flag message template are replaced with the substitutes. <br>
     * The flag message is then returned as a {@link MutableComponent}. <br>
     *
     * @param result      the flag check result to build the message for
     * @param substitutes the substitutes to replace the matches in the flag message template with
     * @return the flag message for the given flag check result and substitutes
     */
    public static MutableComponent buildFrom(FlagCheckResult result, Map<String, String> substitutes) {
        String key = "flag.msg.deny." + result.getFlag().getName();
        Object[] args = new Object[9]; // always 9 slots
        for (var entry : TOKEN_INDEX.entrySet()) {
            String token = entry.getKey();
            int idx = entry.getValue() - 1; // zero-based for array
            args[idx] = substitutes.getOrDefault(token, "");
        }

        return Component.translatable(key, args);
    }



    /**
     * Returns the flag message template for the given flag from the I18n keys. <br>
     * If the flag has a custom message defined, that message is returned instead. <br>
     *
     * @param result of the flag check to get the default message template for
     * @return the default flag message template for the given flag
     */
    private static String getI18nFlagMsgTemplate(FlagCheckResult result) {
        String flagMsgLangKey = "flag.msg.deny." + result.getFlag().getName();
        String fallBackLangKey = "flag.msg.deny." + result.getResponsible().getRegionType().type + ".default";
        return Component.translatableWithFallback(flagMsgLangKey, fallBackLangKey).getString();
    }

    /**
     * Replaces the matches in the given flag message template with the substitutes. <br>
     * The matches are replaced with the substitutes in the given map. <br>
     * The flag message with the matches replaced is then returned. <br>
     *
     * @param flagMsgTemplate the flag message template to replace the matches in
     * @param substitutes     the substitutes to replace the matches with
     * @return the flag message with the matches replaced
     */
    private static String replaceMatches(String flagMsgTemplate, Map<String, String> substitutes) {
        String flagMsg = flagMsgTemplate;
        for (Entry<String, String> entry : substitutes.entrySet()) {
            flagMsg = flagMsg.replace(entry.getKey(), entry.getValue());
        }
        return flagMsg;
    }

    public boolean isMuted() {
        return this.muted;
    }

    public void mute(boolean mute) {
        this.muted = mute;
    }

    public void reset() {
        this.isDefault = true;
        this.msg = CONFIG_MSG;
    }

    public boolean isDefault() {
        return this.msg.equals(CONFIG_MSG);
    }

    public String msg() {
        return msg;
    }

    public void setMsg(String msg) {
        this.msg = msg;
    }

    @Override
    public String toString() {
        return msg;
    }
}
