package de.z0rdak.yawp.util;

import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.stick.AbstractStick;
import de.z0rdak.yawp.core.stick.MarkerStick;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.ListNBT;
import net.minecraft.nbt.StringNBT;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.text.IFormattableTextComponent;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.world.World;

import javax.annotation.Nullable;
import java.util.Objects;

import static net.minecraft.util.text.TextFormatting.*;

public final class StickUtil {

    private StickUtil() {
    }

    public static final String MARKED_BLOCKS = "blocks";
    public static final String VALID_AREA = "valid";
    public static final String AREA_TYPE = "type";
    public static final String DIM = "dim";
    public static final String TP_POS = "tp_pos";
    public static final String IS_TP_SET = "is_tp_set";
    public static final String STICK_TYPE = "stick_type";
    public static final String STICK_ID = "stick-id";
    public static final String STICK = "stick";

    @Nullable
    public static IMarkableArea getMarkedArea(ItemStack stick) {
        if (isVanillaStick(stick) && isMarker(stick)) {
            CompoundNBT stickNBT = StickUtil.getStickNBT(stick);
            if (stickNBT != null) {
                MarkerStick marker = new MarkerStick(stickNBT);
                if (!marker.isValidArea()) {
                    return null;
                }
                return LocalRegions.areaFrom(marker);
            }
        }
        return null;
    }

    public static void applyEnchantmentGlint(ItemStack item) {
        CompoundNBT dummy = new CompoundNBT();
        dummy.putString("id", "");
        dummy.putInt("lvl", 1);
        ListNBT enchantmentList = new ListNBT();
        enchantmentList.add(dummy);
        item.addTagElement("Enchantments", enchantmentList);
    }

    /**
     * Set init (default) nbt value for sticks
     *
     * @param stick stick item
     * @param type  stick type to create
     * @param dim   dimension tag to set for sticks
     */
    public static void initStickTag(ItemStack stick, StickType type, RegistryKey<World> dim) {
        CompoundNBT itemTag = stick.hasTag() ? stick.getTag() : new CompoundNBT();
        if (itemTag != null) {
            if (Objects.requireNonNull(type) == StickType.MARKER) {
                CompoundNBT compoundNBT = new MarkerStick(dim).serializeNBT();
                itemTag.put(STICK, compoundNBT);
                stick.setTag(itemTag);
            }
        }
    }

    public static ItemStack initMarkerNbt(ItemStack stack, StickType type, RegistryKey<World> dim) {
        stack.setCount(1);
        initStickTag(stack, type, dim);
        setStickName(stack, type);
        setStickToolTip(stack, type);
        applyEnchantmentGlint(stack);
        return stack;
    }

    public static boolean isVanillaStick(ItemStack itemStack) {
        return itemStack.getItem().getDefaultInstance().getDescriptionId().equals(Items.STICK.getDescriptionId());
    }

    public static AbstractStick getStick(ItemStack stick) throws StickException {
        if (stick.getTag() != null && stick.hasTag()) {
            if (stick.getTag().contains(STICK)) {
                CompoundNBT stickNbt = stick.getTag().getCompound(STICK);
                StickType type = StickType.of(stickNbt.getString(STICK_TYPE));
                switch (type) {
                    case MARKER:
                        return new MarkerStick(stickNbt);
                    case UNKNOWN:
                    default:
                        throw new StickException("Unknown stick type: '" + type + "'!");
                }
            }
        }
        throw new StickException("Invalid or missing NBT data for Stick '" + stick.getDisplayName().getString() + "'!");
    }

    public static StickType getStickType(ItemStack stick) {
        if (stick.getTag() != null && stick.hasTag()) {
            if (stick.getTag().contains(STICK)) {
                CompoundNBT stickNbt = stick.getTag().getCompound(STICK);
                if (stickNbt.contains(STICK_TYPE)) {
                    return StickType.of(stickNbt.getString(STICK_TYPE));
                }
            }
        }
        return StickType.UNKNOWN;
    }

    public static boolean isMarker(ItemStack stick) {
        return getStickType(stick) == StickType.MARKER;
    }

    public static CompoundNBT getStickNBT(ItemStack stick) {
        if (stick.getTag() != null && stick.hasTag()
                && stick.getTag().contains(STICK)) {
            return stick.getTag().getCompound(STICK);
        } else {
            return null;
        }
    }

    private static final String MARKED_BLOCK_INDICATOR = "X";
    private static final String UNMARKED_BLOCK_INDICATOR = "#";
    private static final String TP_POS_INDICATOR = "TP";
    private static final TextFormatting MARKED_BLOCK_COLOR = GREEN;
    private static final TextFormatting UNMARKED_BLOCK_COLOR = RED;
    private static final TextFormatting UNMARKED_POS_COLOR = AQUA;

    public static void setStickName(ItemStack stick, StickType type) {
        if (Objects.requireNonNull(type) == StickType.MARKER) {
            MarkerStick marker = new MarkerStick(getStickNBT(stick));
            boolean isTpPosSet = marker.getTeleportPos() != null;
            IFormattableTextComponent markerIndicators = buildRegionMarkerIndicators(marker)
                    .append(" ")
                    .append(buildTpPosIndicator(isTpPosSet));
            IFormattableTextComponent markerHoverName = buildStickName(marker)
                    .append(" ")
                    .append(markerIndicators);
            stick.setHoverName(markerHoverName);
        }
    }

    private static IFormattableTextComponent buildStickName(MarkerStick marker) {
        IFormattableTextComponent stickName = new StringTextComponent(marker.getStickType().stickName).withStyle(GOLD);
        IFormattableTextComponent areaType = new StringTextComponent(" (").append(marker.getAreaType().areaType).append(")");
        return stickName.append(areaType);
    }

    /**
     * @param isMarked
     * @return [X] or [#]
     */
    private static IFormattableTextComponent buildMarkerIndicator(boolean isMarked) {
        String indicator = isMarked ? MARKED_BLOCK_INDICATOR : UNMARKED_BLOCK_INDICATOR;
        TextFormatting color = isMarked ? MARKED_BLOCK_COLOR : UNMARKED_BLOCK_COLOR;
        IFormattableTextComponent indicatorComp = new StringTextComponent(indicator).withStyle(color);
        IFormattableTextComponent closedResetComp = new StringTextComponent("]").withStyle(RESET);
        return new StringTextComponent("[").append(indicatorComp).append(closedResetComp);
    }

    private static IFormattableTextComponent buildTpPosIndicator(boolean isMarked) {
        TextFormatting color = isMarked ? MARKED_BLOCK_COLOR : UNMARKED_POS_COLOR;
        IFormattableTextComponent indicatorComp = new StringTextComponent(TP_POS_INDICATOR).withStyle(color);
        IFormattableTextComponent closedResetComp = new StringTextComponent("]").withStyle(RESET);
        return new StringTextComponent("[").append(indicatorComp).append(closedResetComp);
    }

    /**
     * RegionMarker [x][x] [TP]
     *
     * @param marker
     * @return
     */
    private static IFormattableTextComponent buildRegionMarkerIndicators(MarkerStick marker) {
        IFormattableTextComponent regionMarkerIndicators = new StringTextComponent("");
        int maxBlocks = marker.getAreaType().maxBlocks;
        int amountUnmarked = maxBlocks - marker.getMarkedBlocks().size();
        for (int i = 0; i < marker.getMarkedBlocks().size(); i++) {
            regionMarkerIndicators.append(buildMarkerIndicator(true));
        }
        for (int i = 0; i < amountUnmarked; i++) {
            regionMarkerIndicators.append(buildMarkerIndicator(false));
        }
        return regionMarkerIndicators;
    }

    public static void setStickToolTip(ItemStack stick, StickType type) {
        if (Objects.requireNonNull(type) == StickType.MARKER) {
            setToolTip(stick, getMarkerToolTip());
        }
    }

    public static void setToolTip(ItemStack stack, ListNBT loreNbt) {
        stack.getOrCreateTagElement("display").put("Lore", loreNbt);
    }

    public static boolean hasNonNullTag(ItemStack itemStack) {
        return itemStack.hasTag() && itemStack.getTag() != null;
    }

    private static ListNBT getMarkerToolTip() {
        ListNBT lore = new ListNBT();
        lore.add(buildLoreTextLine(new TranslationTextComponent("help.tooltip.stick.marker.simple.1"), "#ff4d4d"));
        lore.add(buildLoreTextLine(new TranslationTextComponent("help.tooltip.stick.marker.simple.2"), "#ff4d4d"));
        lore.add(buildLoreTextLine(new TranslationTextComponent("help.tooltip.stick.marker.simple.3").withStyle(ITALIC), "#808080"));
        lore.add(buildLoreTextLine(new TranslationTextComponent("help.tooltip.stick.marker.simple.4").withStyle(ITALIC), "#808080"));
        return lore;
    }

    private static StringNBT buildLoreTextLine(String text, String hexColor) {
        return StringNBT.valueOf("{\"text\":\"" + text + "\", \"color\":\"" + hexColor + "\"}");
    }

    private static StringNBT buildLoreTextLine(IFormattableTextComponent text, String hexColor) {
        return buildLoreTextLine(text.getString(), hexColor);
    }

}
