package de.z0rdak.yawp.util.text.messages;

import de.z0rdak.yawp.api.events.flag.FlagCheckResult;
import de.z0rdak.yawp.core.flag.RegionFlag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.Map;

import static de.z0rdak.yawp.util.ChatComponentBuilder.tinyBlockPos;

public class FlagMessageBuilder {
    public static final String FLAG_TEMPLATE = "%1$s";
    public static final String POS_TEMPLATE = "%2$s";
    public static final String REGION_TEMPLATE = "%3$s";
    public static final String DIM_TEMPLATE = "%4$s";
    public static final String PLAYER_TEMPLATE = "%5$s";
    public static final String GROUP_TEMPLATE = "%6$s";
    public static final String ENTITY_TEMPLATE = "%7$s";
    public static final String BLOCK_ENTITY = "%8$s";
    public static final String TEAM_TEMPLATE = "%9$s";
    private static final Map<String, Integer> TOKEN_INDEX;

    static {
        TOKEN_INDEX = Map.of(
                FLAG_TEMPLATE, 1,
                POS_TEMPLATE, 2,
                REGION_TEMPLATE, 3,
                DIM_TEMPLATE, 4,
                PLAYER_TEMPLATE, 5,
                GROUP_TEMPLATE, 6,
                ENTITY_TEMPLATE, 7,
                BLOCK_ENTITY, 8,
                TEAM_TEMPLATE, 9);
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
     * Contract: Flag must not be null. Responsible region must not be null. <br>
     *
     * @param result      the flag check result to build the message for
     * @param substitutes the substitutes to replace the matches in the flag message template with
     * @return the flag message for the given flag check result and substitutes
     */
    public static MutableComponent buildFrom(FlagCheckResult result, @Nullable Map<String, String> substitutes) {
        Map<String, String> msgSubstitutes = FlagMessageBuilder.defaultSubstitutesFor(result);
        if (substitutes != null) {
            msgSubstitutes.putAll(substitutes);
        }
        var key = result.getFlag().getFlagMsg().isDefault()
                ? "flag.msg.deny." + result.getResponsible().getRegionType().type + ".default"
                : result.getFlag().getFlagMsg().msg();
        var args = populateArgs(msgSubstitutes);
        return Component.translatable(key, args);
    }

    private static Object[] populateArgs(Map<String, String> substitutes) {
        var args = new Object[9];
        for (var entry : TOKEN_INDEX.entrySet()) {
            int idx = entry.getValue() - 1; // zero-based for array
            args[idx] = substitutes.getOrDefault(entry.getKey(), "");
        }
        return args;
    }
}
