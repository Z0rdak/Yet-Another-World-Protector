package de.z0rdak.yawp.util;

import de.z0rdak.yawp.constants.serialization.ItemNbtKeys;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.stick.MarkerStick;
import net.minecraft.ChatFormatting;
import net.minecraft.client.renderer.entity.TadpoleRenderer;
import net.minecraft.core.component.DataComponents;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.component.CustomData;
import net.minecraft.world.item.component.ItemLore;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

import static net.minecraft.ChatFormatting.*;
import static net.minecraft.core.component.DataComponents.CUSTOM_DATA;

public final class StickUtil {

    private static final String MARKED_BLOCK_INDICATOR = "X";
    private static final String UNMARKED_BLOCK_INDICATOR = "#";
    private static final String TP_POS_INDICATOR = "TP";
    private static final ChatFormatting MARKED_BLOCK_COLOR = GREEN;
    private static final ChatFormatting UNMARKED_BLOCK_COLOR = RED;
    private static final ChatFormatting UNMARKED_POS_COLOR = AQUA;

    private StickUtil() {
    }

    /**
     * Set init (default) nbt value for sticks
     *
     * @param stick stick item
     * @param dim dimension tag to set for sticks
     */
    public static void initStickTag(ItemStack stick, ResourceKey<Level> dim, boolean reset) {
        CustomData customData = stick.get(CUSTOM_DATA);
        if (customData == null) {
            stick.set(CUSTOM_DATA, CustomData.EMPTY);
        }
        customData = stick.get(CUSTOM_DATA);
        if (customData != null) {
            CompoundTag customDataTag = customData.copyTag();
            if (!customDataTag.contains(ItemNbtKeys.STICK) || reset) {
                CompoundTag compoundNBT = new MarkerStick(dim).serializeNBT();
                customDataTag.put(ItemNbtKeys.STICK, compoundNBT);
                stick.set(CUSTOM_DATA, CustomData.of(customDataTag));
            }
        }
    }

    public static void initMarkerNbt(ItemStack stack, ResourceKey<Level> dim) {
        stack.setCount(1);
        initStickTag(stack, dim, false);
        updateStickMetadata(stack);
    }

    public static void resetMarkerNbt(ItemStack stack, ResourceKey<Level> dim) {
        initStickTag(stack, dim, true);
        updateStickMetadata(stack);
    }

    private static void updateStickMetadata(ItemStack stack) {
        updateStickName(stack);
        stack.set(DataComponents.LORE, buildToolTip());
        stack.set(DataComponents.ENCHANTMENT_GLINT_OVERRIDE, true);
    }


    public static boolean isMarker(ItemStack stack) {
        if (hasCustomDataTag(stack)) {
            CustomData customData = stack.get(CUSTOM_DATA);
            CompoundTag compoundTag = customData.copyTag();
            return compoundTag.contains(ItemNbtKeys.STICK) && customData.copyTag().get(ItemNbtKeys.STICK) != null;
        }
        return false;
    }

    @Nullable
    public static CompoundTag getStickNBT(ItemStack stick) {
        if (stick.get(CUSTOM_DATA).copyTag().contains(ItemNbtKeys.STICK)) {
            return (CompoundTag) stick.get(CUSTOM_DATA).copyTag().get(ItemNbtKeys.STICK);
        }
        return null;
    }

    @Nullable
    public static IMarkableArea getMarkedArea(ItemStack stick) {
        if (isMarker(stick)) {
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

    public static void updateStickName(ItemStack stick) {
        CompoundTag stickNBT = getStickNBT(stick);
        if (stickNBT != null) {
            MarkerStick marker = new MarkerStick(stickNBT);
            boolean isTpPosSet = marker.getTeleportPos() != null;
            MutableComponent markerIndicators = buildRegionMarkerIndicators(marker)
                    .append(" ")
                    .append(buildTpPosIndicator(isTpPosSet));
            MutableComponent markerHoverName = buildStickName(marker)
                    .append(" ")
                    .append(markerIndicators);
            stick.set(DataComponents.CUSTOM_NAME, markerHoverName);
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

    public static boolean hasCustomDataTag(ItemStack itemStack) {
        return itemStack.get(CUSTOM_DATA) != null;
    }

    public static void setMarkerNbt(ItemStack itemStack, CompoundTag markerTag) {
        if (hasCustomDataTag(itemStack)) {
            CompoundTag compoundTag = itemStack.get(CUSTOM_DATA).copyTag();
            compoundTag.put(ItemNbtKeys.STICK, markerTag);
            itemStack.set(CUSTOM_DATA, CustomData.of(compoundTag));
        }       
    }


    private static ItemLore buildToolTip() {
        List<Component> lore = new ArrayList<>();
        lore.add(Component.translatableWithFallback("help.tooltip.stick.marker.simple.1", "Used to mark a new region."));
        lore.add(Component.translatableWithFallback("help.tooltip.stick.marker.simple.2", "Keep the Region Marker in your hand while creating a region!"));
        lore.add(Component.literal(ITALIC + "").append(Component.translatableWithFallback("help.tooltip.stick.marker.simple.3", "Mark a (Cuboid) region by right-clicking the diagonal opposite corner blocks.")));
        lore.add(Component.literal(ITALIC + "").append(Component.translatableWithFallback("help.tooltip.stick.marker.simple.4", "Set a region teleport position by shift-right-clicking a block.")));
        return new ItemLore(lore);
    }
}
