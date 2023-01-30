package de.z0rdak.yawp.handler.stick;

public class StickInteractionHandler {

    private StickInteractionHandler() {
    }

    /*
    public static void onRightClickBlock(PlayerInteractEvent.RightClickBlock event) {
        if (!event.getWorld().isClientSide) {
            PlayerEntity player = event.getPlayer();
            // TODO: Maybe check if player is allowed to mark block
            ItemStack involvedItemStack = event.getItemStack();
            if (!involvedItemStack.equals(ItemStack.EMPTY) && isVanillaStick(involvedItemStack)) {
                StickType stickType = getStickType(involvedItemStack);
                if (Objects.requireNonNull(stickType) == StickType.MARKER) {
                    MarkerStickHandler.onMarkBlock(involvedItemStack, event);
                }
            }
        }
    }

    public static void onCycleMode(PlayerInteractEvent.RightClickItem event) {
        if (!event.getWorld().isClientSide) {
            ItemStack involvedItemStack = event.getItemStack();
            // is some valid mod stick
            if (!involvedItemStack.equals(ItemStack.EMPTY)
                    && hasNonNullTag(involvedItemStack)
                    && involvedItemStack.getNbt().contains(STICK)) {
                HitResult blockLookingAt = event.getPlayer().pick(20.0d, 0.0f, false);
                boolean targetIsAir;
                if (blockLookingAt.getType() == HitResult.Type.BLOCK) {
                    BlockPos blockpos = ((BlockHitResult) blockLookingAt).getBlockPos();
                    BlockState blockstate = event.getWorld().getBlockState(blockpos);
                    targetIsAir = blockstate.getBlock().equals(Blocks.AIR);
                } else {
                    targetIsAir = blockLookingAt.getType() == HitResult.Type.MISS;
                }

                if (event.getPlayer().isShiftKeyDown() && targetIsAir) {
                    StickType stickType = getStickType(involvedItemStack);
                    if (Objects.requireNonNull(stickType) == StickType.MARKER) {
                        // FIXME: cycling mode is disabled for now because there is only one working area type
                        //MarkerStickHandler.onCycleRegionMarker(involvedItemStack);
                    }
                }
            }
        }
    }

    /**
     * Handles action when renaming mod sticks in an anvil.
     * This is used to create a mod stick or to define a region by renaming a valid RegionMarker stick.

    public static void onStickRename(AnvilRepairEvent event) {
        PlayerEntity player = event.getPlayer();
        if (!player.getWorld().isClient) {
            ItemStack outputItem = event.getItemResult();
            ItemStack inputItem = event.getItemInput();
            ItemStack ingredientInput = event.getIngredientInput();
            boolean hasStickTag = outputItem.hasNbt() && outputItem.getNbt() != null && outputItem.getNbt().contains(STICK);
            // TODO: Check if player is allowed to create region -
            // FIXME: this is not possible when player has no rights for dimension - a parent would need to be selected
            // stick set region parent where player == owner
            if (hasStickTag) {
                MarkerStickHandler.onCreateRegion(event);
            }
            boolean isInputAndOutputStick = ItemStack.areEqual(outputItem, Items.STICK.getDefaultStack())
                    && ItemStack.areEqual(inputItem, Items.STICK.getDefaultStack());
            if (isInputAndOutputStick && ingredientInput.isEmpty()) {
                onCreateStick(event);
            }
        }
    }

    /**
     * Edits the NBT data of the renamed stick to "transform" it to the corresponding mod stick.
     * @param event the event data from renaming the stick item

    private static void onCreateStick(AnvilRepairEvent event) {
        PlayerEntity player = event.getPlayer();
        ItemStack outputItem = event.getItemResult();
        ItemStack inputItem = event.getItemInput();
        StickType type = StickType.of(outputItem.getName().getString());
        if (type != StickType.UNKNOWN) {
            // split stack and only create one stick, also refund xp
            inputItem.setCount(outputItem.getCount() - 1);
            player.giveItemStack(inputItem);
            // TODO: Send network packet to force inventory sync
            event.setBreakChance(0.0f);
            player.addExperienceLevels(1);
            initMarkerNbt(outputItem, type, event.getPlayer().getCommandSenderWorld().dimension());
        }
    }

     */
}
