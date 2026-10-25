package de.z0rdak.yawp.util.text.messages;

import de.z0rdak.yawp.api.events.flag.FlagCheckResult;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

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

    private FlagMessageBuilder() {}

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
        if (result.getFlagCheck().getPlayer() != null && result.getFlagCheck().getRegionFlag().isPlayerFlag()) {
            substituteMap.put(PLAYER_TEMPLATE, result.getFlagCheck().getPlayer().getScoreboardName());
        }
        return substituteMap;
    }

    /**
     * Builds a flag denial message from the given flag check result and substitutes.
     * <p>
     * The message is selected according to the following priority:
     * <ol>
     *     <li>If the flag has a custom message (not default), its configured message key is used.</li>
     *     <li>If the flag message is default, the translation key associated with the flag itself is used.</li>
     *     <li>If the selected translation key is missing on the client, the default translation key for the responsible region type is used.</li>
     *     <li>If the region-type default translation key is also missing, a hardcoded fallback message is displayed.</li>
     * </ol>
     * <p>
     * Translation keys are resolved on the client using the client's language resources. The server cannot guarantee that a
     * translation key exists on the client.
     * <p>
     * Default substitutes are generated from the flag check result. Additional substitutes provided by the caller override
     * the defaults when keys overlap. These substitutes are applied to the selected message and its fallbacks.
     * <p>
     * Contract: The flag and responsible region in the result must not be null.
     *
     * @param result      the flag check result to build the message for
     * @param substitutes optional additional substitutes for the message
     * @return the flag denial message as a {@link MutableComponent}
     */
    public static MutableComponent buildFrom(FlagCheckResult result, @Nullable Map<String, String> substitutes) {
        Objects.requireNonNull(result.getResponsible());
        Objects.requireNonNull(result.getFlag());
        Map<String, String> msgSubstitutes = FlagMessageBuilder.defaultSubstitutesFor(result);
        if (substitutes != null) {
            msgSubstitutes.putAll(substitutes);
        }
        var regionType = result.getResponsible().getRegionType();
        final String defaultRegionTypeFlagKey = "flag.msg.deny." + regionType.type + ".default";
        final String playerFlagKey = "flag.msg.deny." + result.getFlag().getName();
        final String flagMessage = result.getFlag().getFlagMsg().msg();
        final String i18nFlagKey = result.getFlag().getFlagMsg().isDefault()
                ? playerFlagKey
                : flagMessage;
        final String fallback = switch (regionType) {
            case GLOBAL -> "This action is globally §cdenied§r by the §b%1$s§r flag!";
            case DIMENSION -> "This action is §cdenied§r in this dimension by the §b%1$s§r flag!";
            case LOCAL -> "[%3$s]: The §b%1$s§r flag §cdenies§r this action in this area!";
            default -> "[%3$s]: The §b%1$s§r flag §cdenies§r this action here!";
        };
        Object[] args = populateArgs(msgSubstitutes);
        var fallbackKeyAndMsg = Component.translatableWithFallback(defaultRegionTypeFlagKey, fallback, args);
        return Component.translatableWithFallback(i18nFlagKey, fallbackKeyAndMsg.getString(), args);
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
