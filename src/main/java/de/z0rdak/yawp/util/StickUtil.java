package de.z0rdak.yawp.util;

import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.stick.AbstractStick;
import de.z0rdak.yawp.core.stick.MarkerStick;
import net.minecraft.ChatFormatting;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.StringTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.Level;

import javax.annotation.Nullable;
import java.util.Objects;

import static net.minecraft.ChatFormatting.*;

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

    private static final String MARKED_BLOCK_INDICATOR = "X";

    public static void applyEnchantmentGlint(ItemStack item) {
        CompoundTag dummy = new CompoundTag();
        dummy.putString("id", "");
        dummy.putInt("lvl", 1);
        ListTag enchantmentList = new ListTag();
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
    public static void initStickTag(ItemStack stick, StickType type, ResourceKey<Level> dim) {
        CompoundTag itemTag = stick.hasTag() ? stick.getTag() : new CompoundTag();
        if (itemTag != null) {
            if (Objects.requireNonNull(type) == StickType.MARKER) {
                CompoundTag compoundNBT = new MarkerStick(dim).serializeNBT();
                itemTag.put(STICK, compoundNBT);
                stick.setTag(itemTag);
            }
        }
    }

    public static ItemStack initMarkerNbt(ItemStack stack, StickType type, ResourceKey<Level> dim) {
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
                CompoundTag stickNbt = stick.getTag().getCompound(STICK);
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
                CompoundTag stickNbt = stick.getTag().getCompound(STICK);
                if (stickNbt.contains(STICK_TYPE)) {
                    return StickType.of(stickNbt.getString(STICK_TYPE));
                }
            }
        }
        return StickType.UNKNOWN;
    }

    private static final String UNMARKED_BLOCK_INDICATOR = "#";

    public static CompoundTag getStickNBT(ItemStack stick) {
        if (stick.getTag() != null && stick.hasTag()
                && stick.getTag().contains(STICK)) {
            return stick.getTag().getCompound(STICK);
        } else {
            return null;
        }
    }

    private static final String TP_POS_INDICATOR = "TP";
    private static final ChatFormatting MARKED_BLOCK_COLOR = GREEN;
    private static final ChatFormatting UNMARKED_BLOCK_COLOR = RED;
    private static final ChatFormatting UNMARKED_POS_COLOR = AQUA;

    @Nullable
    public static IMarkableArea getMarkedArea(ItemStack stick) {
        if (isVanillaStick(stick) && isMarker(stick)) {
            CompoundTag stickNBT = StickUtil.getStickNBT(stick);
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

    public static boolean isMarker(ItemStack stick) {
        return getStickType(stick) == StickType.MARKER;
    }

    public static void setStickName(ItemStack stick, StickType type) {
        if (Objects.requireNonNull(type) == StickType.MARKER) {
            MarkerStick marker = new MarkerStick(getStickNBT(stick));
            boolean isTpPosSet = marker.getTeleportPos() != null;
            MutableComponent markerIndicators = buildRegionMarkerIndicators(marker)
                    .append(" ")
                    .append(buildTpPosIndicator(isTpPosSet));
            MutableComponent markerHoverName = buildStickName(marker)
                    .append(" ")
                    .append(markerIndicators);
            stick.setHoverName(markerHoverName);
        }
    }

    private static MutableComponent buildStickName(MarkerStick marker) {
        MutableComponent stickName = Component.literal(marker.getStickType().stickName).withStyle(GOLD);
        MutableComponent areaType = Component.literal(" (").append(marker.getAreaType().areaType).append(")");
        return stickName.append(areaType);
    }

    /**
     * @param isMarked
     * @return [X] or [#]
     */
    private static MutableComponent buildMarkerIndicator(boolean isMarked) {
        String indicator = isMarked ? MARKED_BLOCK_INDICATOR : UNMARKED_BLOCK_INDICATOR;
        ChatFormatting color = isMarked ? MARKED_BLOCK_COLOR : UNMARKED_BLOCK_COLOR;
        MutableComponent indicatorComp = Component.literal(indicator).withStyle(color);
        MutableComponent closedResetComp = Component.literal("]").withStyle(RESET);
        return Component.literal("[").append(indicatorComp).append(closedResetComp);
    }

    private static MutableComponent buildTpPosIndicator(boolean isMarked) {
        ChatFormatting color = isMarked ? MARKED_BLOCK_COLOR : UNMARKED_POS_COLOR;
        MutableComponent indicatorComp = Component.literal(TP_POS_INDICATOR).withStyle(color);
        MutableComponent closedResetComp = Component.literal("]").withStyle(RESET);
        return Component.literal("[").append(indicatorComp).append(closedResetComp);
    }

    /**
     * RegionMarker [x][x] [TP]
     *
     * @param marker
     * @return
     */
    private static MutableComponent buildRegionMarkerIndicators(MarkerStick marker) {
        MutableComponent regionMarkerIndicators = Component.literal("");
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

    public static void setToolTip(ItemStack stack, ListTag loreNbt) {
        stack.getOrCreateTagElement("display").put("Lore", loreNbt);
    }

    public static boolean hasNonNullTag(ItemStack itemStack) {
        return itemStack.hasTag() && itemStack.getTag() != null;
    }

    private static ListTag getMarkerToolTip() {
        ListTag lore = new ListTag();
        lore.add(buildLoreTextLine(Component.translatableWithFallback("help.tooltip.stick.marker.simple.1", "Used to mark a new region."), "#ff4d4d"));
        lore.add(buildLoreTextLine(Component.translatableWithFallback("help.tooltip.stick.marker.simple.2", "Keep the Region Marker in your hand while creating a region!"), "#ff4d4d"));
        lore.add(buildLoreTextLine(Component.literal(ITALIC + "").append(Component.translatableWithFallback("help.tooltip.stick.marker.simple.3", "Mark a (Cuboid) region by right-clicking the diagonal opposite corner blocks.")), "#808080"));
        lore.add(buildLoreTextLine(Component.literal(ITALIC + "").append(Component.translatableWithFallback("help.tooltip.stick.marker.simple.4", "Set a region teleport position by shift-right-clicking a block.")), "#808080"));
        return lore;
    }

    private static StringTag buildLoreTextLine(String text, String hexColor) {
        return StringTag.valueOf("{\"text\":\"" + text + "\", \"color\":\"" + hexColor + "\"}");
    }

    private static StringTag buildLoreTextLine(MutableComponent text, String hexColor) {
        return buildLoreTextLine(text.getString(), hexColor);
    }

}
